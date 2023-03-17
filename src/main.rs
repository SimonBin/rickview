#![warn(clippy::pedantic)]
#![allow(clippy::wildcard_imports)]
#![allow(clippy::enum_glob_use)]
#![allow(clippy::unused_async)]
#![allow(clippy::similar_names)]
#![feature(once_cell)]
#![feature(let_chains)]
#![feature(iter_collect_into)]
//! Lightweight and performant RDF browser.
//! An RDF browser is a web application that *resolves* RDF resources: given the HTTP(s) URL identifying a resource it returns an HTML summary.
//! Besides HTML, the RDF serialization formats RDF/XML, Turtle and N-Triples are also available using content negotiation.
//! Default configuration is stored in `data/default.toml`, which can be overriden in `data/config.toml` or environment variables.
//! Configuration keys are in `lower\_snake\_ca`se, while environment variables are prefixed with RICKVIEW\_ and are `in SCREAMING\_SNAKE\`_CASE.
mod about;
/// The main module uses Actix Web to serve resources as HTML and other formats.
mod config;
mod rdf;
mod resource;

use crate::{
    config::config,
    rdf::{namespace, Piri},
    resource::Resource,
};
use about::About;
use actix_web::{
    get, head,
    http::header::{self, ETag, EntityTag},
    web,
    web::scope,
    App, HttpRequest, HttpResponse, HttpServer, Responder,
};
use log::{debug, error, info, trace, warn};
use once_cell::sync::Lazy;
use sophia::{iri::Iri, term::{SimpleIri, TTerm}};
use std::{
    collections::hash_map::DefaultHasher,
    hash::{Hash, Hasher},
    time::Instant,
};
use tinytemplate::TinyTemplate;

static RESOURCE: &str = std::include_str!("../data/resource.html");
static FAVICON: &[u8; 1150] = std::include_bytes!("../data/favicon-aksw.ico");
static FAVICON_HASH: Lazy<EntityTag> = Lazy::new(|| {
    let mut hasher = DefaultHasher::new();
    hasher.write(FAVICON);
    EntityTag::new_strong(format!("{:e}", hasher.finish()))
});
static CSS: &str = std::include_str!("../data/rickview.css");
static CSS_HASH: Lazy<EntityTag> = Lazy::new(|| {
    let mut hasher = DefaultHasher::new();
    CSS.hash(&mut hasher);
    EntityTag::new_strong(format!("{:e}", hasher.finish()))
});
//static INDEX: &str = std::include_str!("../data/index.html");
static ABOUT: &str = std::include_str!("../data/about.html");

fn template() -> TinyTemplate<'static> {
    let mut tt = TinyTemplate::new();
    tt.add_template("resource", RESOURCE).expect("Could not parse resource page template");
    //tt.add_template("index", INDEX).expect("Could not parse index page template");
    tt.add_template("about", ABOUT).expect("Could not parse about page template");
    tt.add_formatter("uri_to_suffix", |json, output| {
        let o = || -> Option<String> {
            let s = json.as_str().unwrap_or_else(|| panic!("JSON value is not a string: {json}"));
            let mut s = s.rsplit_once('/').unwrap_or_else(|| panic!("no '/' in URI '{s}'")).1;
            if s.contains('#') {
                s = s.rsplit_once('#')?.1;
            }
            Some(s.to_owned())
        };
        output.push_str(&o().unwrap());
        Ok(())
    });
    tt.add_formatter("uri_to_local", |json, output| {
        let o = || -> Option<String> {
            let s = json.as_str().unwrap_or_else(|| panic!("JSON value is not a string: {json}"));
            Some(if !s.contains(":") {
                s.to_string()
            } else if s.starts_with(&config().namespace) && s != &config().namespace && !s.contains("#") {
                s.replace(&config().namespace, "/")
            } else {
                String::from("/?") + &s.replace("#", "%23")
            })
        };
        output.push_str(&o().unwrap());
        Ok(())
    });
    tt.add_formatter("uri_to_prefixed", |json, output| {
        let o = || -> Option<String> {
            let s = json.as_str().unwrap_or_else(|| panic!("JSON value is not a string: {json}"));
            Some(match Iri::new(&s) {
                Ok(iri) => Piri::new(iri.boxed()).prefixed_string(true, true),
                _ => String::from(s),
            })
        };
        output.push_str(&o().unwrap());
        Ok(())
    });
    tt.add_formatter("count", |json, output| {
        let o = || -> Option<String> {
            let s = json.as_array().unwrap_or_else(|| panic!("JSON value is not a array: {json}"));
            Some(s.len().to_string())
        };
        output.push_str(&o().unwrap());
        Ok(())
    });
    tt
}

#[head("{_anypath:.*}")]
async fn head() -> HttpResponse { HttpResponse::NotImplemented().finish() }

#[get("{_anypath:.*/|}rickview.css")]
async fn css(request: HttpRequest) -> impl Responder {
    match request.headers().get(header::IF_NONE_MATCH) {
        Some(e) => {
            if e == &CSS_HASH.to_string() {
                return HttpResponse::NotModified().finish();
            }
        }
        None => {}
    };
    HttpResponse::Ok().content_type("text/css").append_header(ETag(CSS_HASH.clone())).body(CSS)
}

#[get("{_anypath:.*/|}favicon.ico")]
async fn favicon(request: HttpRequest) -> impl Responder {
    match request.headers().get(header::IF_NONE_MATCH) {
        Some(e) => {
            if e == &FAVICON_HASH.to_string() {
                return HttpResponse::NotModified().finish();
            }
        }
        None => {}
    };
    HttpResponse::Ok().content_type("image/x-icon").append_header(ETag(FAVICON_HASH.clone())).body(FAVICON.as_ref())
}

#[get("{suffix:.*|}")]
async fn res_html(request: HttpRequest, suffix: web::Path<String>) -> impl Responder {
    let force_html = suffix.ends_with(".html");
    let suff = if force_html { suffix.strip_suffix(".html").unwrap() } else { &suffix };
    let page = if suff == "Projects" {
	SimpleIri::new("http://xmlns.com/foaf/0.1/Project", None)
    } else if suff == "Groups" {
	SimpleIri::new("http://purl.org/vocab/aiiso/schema#ResearchGroup", None)
    } else {
	namespace().get(suff)
    }.unwrap();
    res_html_common(request, &page, force_html)
}

#[get("robots.txt")]
async fn robots(_request: HttpRequest) -> impl Responder {
    HttpResponse::Ok().content_type("text/plain").body("User-agent: *
Disallow:
")
}

enum ContentType {
    Html,
    #[cfg(feature = "rdfxml")]
    Rdfxml,
    Ttl,
    Nt,
}

fn get_content_type(request: HttpRequest, force_html: bool) -> ContentType {
    if force_html { return ContentType::Html; }
    
    match request.head().headers().get("Accept") {
        Some(a) => {
            if let Ok(accept) = a.to_str() {
                trace!("accept header {}", accept);
                if accept.contains("text/html") {
		    return ContentType::Html;
		}
                if accept.contains("application/n-triples") {
		    return ContentType::Nt;
		}
                #[cfg(feature = "rdfxml")]
                if accept.contains("application/rdf+xml") {
		    return ContentType::Rdfxml;
                }
	    }
	}
        None => {
            trace!("accept header missing, using RDF Turtle");
        }
    }
    return ContentType::Ttl;
}

fn res_html_common(request: HttpRequest, resource: &SimpleIri<'_>, force_html: bool) -> HttpResponse {
    let t = Instant::now();
    let local_suffix = resource.value().replace(&config().namespace, "");
    let prefixed = match Iri::new(&resource.to_string()) {
        Ok(iri) => Piri::new(iri.boxed()).short(),
        _ => resource.to_string(),
    };
    let content_type = get_content_type(request, force_html);
    match rdf::resource(resource) {
        Err(_) => {
            let message = format!("No triples found for resource {prefixed}");
            warn!("{}", message);
            match content_type {
		ContentType::Html => {
		    if let Some(noslash) = local_suffix.strip_suffix("/") {
			if let Ok(iri) = namespace().get(&noslash) {
			    if let Ok(_) = rdf::resource(&iri) {
				return HttpResponse::TemporaryRedirect().append_header(("location", format!("/{}", noslash))).finish()
			    }
			}
		    }
                    match template().render(
                        "resource",
                        &Resource {
			    jsonld: None,
                            suffix: local_suffix.clone(),
                            title: "404 Not found".to_string(),
                            title_maybe_link: "404 Not found".to_string(),
                            descriptions: vec![("Warning".to_string(), vec![message])],
                            directs: vec![],
                            inverses: vec![],
			    superclasses: vec![],
			    subclasses: vec![],
			    instances: vec![],
                            duration: "".to_string(),
                            uri: resource.value().to_string(),
                            main_type: None,
                            github_issue_url: config().github.as_ref().map(|g| format!("{g}/issues/new?title={local_suffix}")),
                            depiction: None,
                            bibtag: None,
                            edit_url: None,
			    redirect: None,
                        },
                    ) {
                        Ok(html) => {
                            debug!("{} HTML {:?}", prefixed, t.elapsed());
                            return HttpResponse::NotFound().content_type("text/html; charset-utf-8").body(html)
                        }
                        Err(err) => {
                            let message = format!("Internal server error. Could not render resource {prefixed}:\n{err}.");
                            error!("{}", message);
                            return HttpResponse::InternalServerError().body(message)
                        }
                    };
                }
		ContentType::Nt => {
                    return HttpResponse::NotFound().content_type("application/n-triples").body(rdf::serialize_nt(resource));
                }
                #[cfg(feature = "rdfxml")]
		ContentType::Rdfxml => { 
                    return HttpResponse::NotFound().content_type("application/rdf+xml").body(rdf::serialize_rdfxml(resource));
                }
		ContentType::Ttl => {
		    return HttpResponse::NotFound().content_type("application/turtle").body(rdf::serialize_turtle(resource))
		}
            }
        }
        Ok(res) => {
	    match content_type {
		ContentType::Html => {
                    return match template().render("resource", &res) {
                        Ok(html) => {
                            debug!("{} HTML {:?}", prefixed, t.elapsed());
                            HttpResponse::Ok().content_type("text/html; charset-utf-8").body(html)
                        }
                        Err(err) => {
                            let message = format!("Internal server error. Could not render resource {prefixed}:\n{err}.");
                            error!("{}", message);
                            HttpResponse::InternalServerError().body(message)
                        }
                    };
                }
		ContentType::Nt => {
                    debug!("{} N-Triples {:?}", prefixed, t.elapsed());
                    return HttpResponse::Ok().content_type("application/n-triples").body(rdf::serialize_nt(resource));
                }
                #[cfg(feature = "rdfxml")]
		ContentType::Rdfxml => { 
                    debug!("{} RDF {:?}", prefixed, t.elapsed());
                    return HttpResponse::Ok().content_type("application/rdf+xml").body(rdf::serialize_rdfxml(resource));
                }
		ContentType::Ttl => {
		    debug!("{} RDF Turtle {:?}", prefixed, t.elapsed());
		    return HttpResponse::Ok().content_type("application/turtle").body(rdf::serialize_turtle(resource))
		}
            }
	}
    }
}

#[get("/")]
async fn index(request: HttpRequest) -> impl Responder { index_impl(request) }

fn index_impl(request: HttpRequest) -> HttpResponse {
    if !request.query_string().is_empty() {
        let qs = String::from(request.query_string().replace("%23", "#"));
        let iri = SimpleIri::new(&qs, None);
        if iri.is_ok() {
            return res_html_common(request, &iri.unwrap(), false);
        }
        let e = iri.unwrap_err();
        let message = format!("Could not render resource: {e:?}");
        error!("{}", message);
        return HttpResponse::NotAcceptable().body(message);
    }
    HttpResponse::TemporaryRedirect().append_header(("location", "/About")).finish()
}

#[get("/about")]
async fn about_page() -> impl Responder {
    match template().render("about", &About::new()) {
        Ok(body) => HttpResponse::Ok().content_type("text/html").body(body),
        Err(e) => {
            let message = format!("Could not render about page: {e:?}");
            error!("{}", message);
            HttpResponse::InternalServerError().body(message)
        }
    }
}

// redirect /base to correct index page /base/
#[get("")]
async fn redirect() -> impl Responder { HttpResponse::TemporaryRedirect().append_header(("location", config().base.clone() + "/")).finish() }

#[actix_web::main]
async fn main() -> std::io::Result<()> {
    trace!("{:?}", config());
    info!("Serving {} at http://localhost:{}{}/", config().namespace, config().port + 1, config().base);
    HttpServer::new(move || {
        App::new()
            .service(head)
	    .service(css)
	    .service(favicon)
	    .service(robots)
	    .service(scope(&config().base)
		     .service(index)
		     .service(about_page)
		     .service(redirect)
		     .service(res_html))
    })
    .bind(("0.0.0.0", config().port + 1))?
    .run()
    .await
}
