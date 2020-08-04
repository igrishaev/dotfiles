
{:auth {:repository-auth
        {#"artifacts.exoscale.ch"
         {:username ""
          :password ""}}}

 :user {:dependencies [[org.clojure/tools.nrepl "0.2.13"]]

        :signing {:gpg-key "ivan@grishaev.me"}

        :global-vars {*warn-on-reflection* true
                      *assert* true}

        :injections [(require 'clojure.pprint)
                     (require 'clojure.inspector)]

        :plugins [[cider/cider-nrepl "0.25.0"]]}}
