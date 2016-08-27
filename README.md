# Cambo

A combo of Falcor, Relay, and Om Next written in Clojure(Script).  A cambo if you will.

## Should I use this?

Only for non-critical applications.  It is ready for trying out and we are using it for internal tools, but the API is
in flux and there will be areas of functionality we haven't hit yet which are probably broken.

Any issues are super welcome.  We want to expand our use of this and so bug reports are A+.

## What

Cambo is three things:

- A router which runs on the server and is able to expose a single schema to clients which is composed of multiple backend
services.  When a client request is received the router dispatches to each backend service, collects the results, and
returns it back to the client.

- A model which runs on the client or server and is able to cache results and diff queries against this cache.  When a
client requests data it does so through the model which can either answer immediately if the data is cached, send a
partial request if the cache contains some of the data, or send a full request if the cache contains none of the data.

- A react component system which allows components to declare their data requirements, compose those data requirements
via the component hierarchy, and interface with the model to satisfy those data requirements before being mounted.

Each piece can be used independently -- which is a goal -- but they are all contained in a single repository because
I am lazy.

The router and model are inspired (aka stolen) from Falcor while the component model is inspired (again, stolen) from
Relay.  The use of datomic pull-like queries was inspired by Om Next as well as general 'how does clojurescript work'
ideas.

## Why

There is a general movement for having part of your client -- be it web or native -- running on the server (see:
[Backend for Frontends](http://samnewman.io/patterns/architectural/bff/)).  This might initially be a performance
optimization -- reducing network requests -- but the real benefit is in developer productivity.

Relay was introduced via a talk highlighting the productivity benefits.  When client data requirements change any
individual component need only change its declared data dependencies.  These data requirements flow up the component
hierarchy, and then data flows back down when rendered.  If the server exposes it any component can require the data.
The fact that these data requirements can be collected and sent as a single request are just an optimization.

## Concepts

### Paths

The key concept in all of this is the idea of a path.  A path is just a vector of keys describing the location of a
piece of data within a map.  Think `get-in`.

```clj
(def graph {:user/by-id {1 {:user/name "Huey"}}})

(get-in graph [:user/by-id 1 :user/name])
;; => "Huey"
```

In the above example the path is `[:user/by-id 1 :user/name]` and the data at the location described by the path is
`"Huey"`.

Most applications aren't pulling data from a map but from a database, web service, etc.  That said, you can still
describe each piece of data with a unique path location.  Lets take the github api as an example.  I want to know
the description of the Netflix organization [api](https://api.github.com/orgs/netflix).  A path for this datum could be
`[:org/by-name "Netflix" :org/description]` (I used a Rich Hickey word in this sentence -- achievement unlocked).

If we modeled the github api as a map I could execute the following line to get the description:

```clj
(get-in github-api [:org/by-name "Netflix" :org/description])
;; => "Netflix Open Source Platform"
```

Being able to treat a backend as a map is exactly what the Cambo Router allows.
How to use the router to do this will be described later.

### Queries

A query is simply a concise way to express multiple paths.  Building on the above example we can also add the data
requirement of the organizations location.

```clj
(get-in github-api [:org/by-name "Netflix" :org/description])
;; => "Netflix Open Source Platform"
(get-in github-api [:org/by-name "Netflix" :org/location])
;; => "Los Gatos, California"
```

Instead of getting multiple paths individually we can use the pull syntax to do it as a single operation.

```clj
(pull github-api [{:org/by-name [{"Netflix" [:org/description
                                             :org/location]}]}])
;; => {:org/by-name {"Netflix" {:org/description "Netflix Open Source Platform"
;;                              :org/location "Los Gatos, California"}}}
```

### Fragments

Sometimes a component doesn't know the identity of its data requirements.  For instance a react component might know
it needs `:org/description` and `:org/location` but doesn't know (or care) which organization -- some parent will
figure that stuff out.  It can still express its data requirements using the query syntax, but this query is not
rooted at an identity.

```clj
;; no identity, what org? ... no idea how to satisify this!
(pull github-api [:org/description :org/location])
;; => nil
```

Luckily we can easily compose a fragment with a path to get a rooted query which has identity.

```clj
(prepend-query [:org/by-name "Netflix"] [:org/description
                                         :org/location])
;; => [{:org/by-name [{"Netflix" [:org/description
;;                                :org/location]}]}]
```

This composition of fragments is key to building up full queries via small component-local data requirements.

### Graph

TODO: ref / atom

### Router

TODO

```clj
(def org-route
  {:route [:org/by-id INTEGERS [:org/description
                                :org/email
                                :org/name
                                :org/login
                                :org/id
                                :org/url]]
   :get (fn [[_ ids keys] _]
          (for [id ids
                :let [org (api-get (str "/organizations/" id))]
                :when org
                key keys
                :let [github-key (keyword (name key))]]
            (path-value [:org/by-id id key]
                        (get org github-key))))})
```



### Containers

TODO

```clj
(defcontainer OrganizationLink
  :fragments {:org [:org/url]}
  (render [this]
          (let [{:keys [url]} (props this :org)]
            (link {:href url} (children this)))))

(def org-link (factory OrganizationLink))

(defcontainer OrganizationHeader
  :fragments {:org [:org/name
                    :org/description
                    (get-fragment OrganizationLink :org)]}
  (render [this]
          (let [{:keys [name description] :as org} (props this :org)]
            (div
              (h1 nil name)
              (p nil description)
              (org-link {:org org} "details")))))

(def org-header (factory OrganizationHeader))
```

### Renderers

TODO

```clj
(def model (model/model {:datasource (http-datasource "http://localhost:4000/cambo"
                                                      {"X-CSRF-TOKEN" "abc123"})}))

(js/ReactDOM.render
  (renderer {:queries {:org [:org/by-name "Netflix"]}
             :container OrganizationHeader
             :model model})
  (.getElementById js/document "app"))
```

### Model

TODO


## License

Copyright © 2016 Erik Petersen

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
