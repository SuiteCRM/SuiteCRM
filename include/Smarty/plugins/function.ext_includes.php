<?php

/*

Modification information for LGPL compliance

r56990 - 2010-06-16 13:05:36 -0700 (Wed, 16 Jun 2010) - kjing - snapshot "Mango" svn branch to a new one for GitHub sync

r56989 - 2010-06-16 13:01:33 -0700 (Wed, 16 Jun 2010) - kjing - defunt "Mango" svn dev branch before github cutover

r55980 - 2010-04-19 13:31:28 -0700 (Mon, 19 Apr 2010) - kjing - create Mango (6.1) based on windex

r51719 - 2009-10-22 10:18:00 -0700 (Thu, 22 Oct 2009) - mitani - Converted to Build 3  tags and updated the build system 

r51634 - 2009-10-19 13:32:22 -0700 (Mon, 19 Oct 2009) - mitani - Windex is the branch for Sugar Sales 1.0 development

r50375 - 2009-08-24 18:07:43 -0700 (Mon, 24 Aug 2009) - dwong - branch kobe2 from tokyo r50372

r47412 - 2009-05-20 17:50:30 -0700 (Wed, 20 May 2009) - xye - Bug30528 Merged the fix for bug 30528 into Tokyo RC.
Modified:
modules/ModuleBuilder/tpls/includes.tpl
modules/Configurator/EditView.tpl
include/Smarty/plugins/function.ext_includes.php

r42807 - 2008-12-29 11:16:59 -0800 (Mon, 29 Dec 2008) - dwong - Branch from trunk/sugarcrm r42806 to branches/tokyo/sugarcrm

r32672 - 2008-03-11 18:48:38 -0700 (Tue, 11 Mar 2008) - dwheeler - Added new Field Types to Studio/Module Builder.
iFrame, Displays a dynamic or static URL in an iFrame in the detail view of a record.
dynamicURL, You can generate a url based on the properties of a record. Such as displaying a link to the shipping location of an account on google maps. 

r32667 - 2008-03-11 17:16:04 -0700 (Tue, 11 Mar 2008) - majed - changes for templating

r31712 - 2008-02-11 18:44:00 -0800 (Mon, 11 Feb 2008) - awu - fixing END sugarcrm int tag
Touched:
- include/Smarty/plugins/function.ext_includes.php

r31615 - 2008-02-05 10:52:49 -0800 (Tue, 05 Feb 2008) - dwheeler - Added Ext theming abilities as well as an ext_includes smarty function.


*/



function smarty_function_ext_includes($params, &$smarty)
{
	$ret = '<link rel="stylesheet" type="text/css" href="' . getJSPath("themes/default/ext/resources/css/ext-all.css") . '" />'
		 . '<link rel="stylesheet" type="text/css" href="' . getJSPath("themes/default/ext/resources/css/xtheme-gray.css") . '" />';
		 
	global $theme;
	if (is_dir("themes/$theme/ext/resources/css")) {
			$cssDir = opendir("themes/$theme/ext/resources/css");
			while (($file = readdir($cssDir)) !== false) {
				if (strcasecmp(substr($file, -4), '.css') == 0) {
            		$ret .= "<link rel='stylesheet' type='text/css' href='" . getJSPath("themes/$theme/ext/resources/css/$file") . "' />";
				}
        	}
	}
			
	return $ret;
	
}