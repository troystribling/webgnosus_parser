{application, webgnosus, 
 [{description, "webgnosus microblog analysis"},
  {vsn, "1.0"},
  {modules, [
             webgnosus, 
             webgnosus_supervisor, 
             webgnosus_events, 
             webgnosus_http, 
             laconica_interface, 
             laconica_server
            ]},	
  {registered, [
                webgnosus_interface, 
                webgnosus_supervisor
               ]},
  {applications, [
                  kernel,
                  stdlib,
                  inets
                 ]},
  {mod, {webgnosus,[]}},
  {start_phases, []}
 ]}.
