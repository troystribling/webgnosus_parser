{application, tweeter, 
 [{description, "twitter analysis"},
  {vsn, "1.0"},
  {modules, [tweeter, tweeter_supervisor, tweeter_interface]},	
  {registered,[tweeter_interface, tweeter_supervisor]},
  {applications, [kernel,stdlib]},
  {mod, {tweeter,[]}},
  {start_phases, []}
 ]}.
