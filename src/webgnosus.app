{application, webgnosus, 
 [{description, "webgnosus microblog analysis"},
  {vsn, "1.0"},
  {modules, [webgnosus, webgnosus_supervisor, laconica_interface, laconica_server]},	
  {registered,[webgnosus_interface, webgnosus_supervisor]},
  {applications, [kernel,stdlib]},
  {mod, {webgnosus,[]}},
  {start_phases, []}
 ]}.
