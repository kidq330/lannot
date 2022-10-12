{ mk_plugin }:

mk_plugin {
  plugin-name = "lannotate" ;
  plugin-src = fetchGit { shallow=true ; url=./.. ; } ;
}
