<?php

/*

Modification information for LGPL compliance

r56990 - 2010-06-16 13:05:36 -0700 (Wed, 16 Jun 2010) - kjing - snapshot "Mango" svn branch to a new one for GitHub sync

r56989 - 2010-06-16 13:01:33 -0700 (Wed, 16 Jun 2010) - kjing - defunt "Mango" svn dev branch before github cutover

r55980 - 2010-04-19 13:31:28 -0700 (Mon, 19 Apr 2010) - kjing - create Mango (6.1) based on windex

r51719 - 2009-10-22 10:18:00 -0700 (Thu, 22 Oct 2009) - mitani - Converted to Build 3  tags and updated the build system 

r51634 - 2009-10-19 13:32:22 -0700 (Mon, 19 Oct 2009) - mitani - Windex is the branch for Sugar Sales 1.0 development

r50375 - 2009-08-24 18:07:43 -0700 (Mon, 24 Aug 2009) - dwong - branch kobe2 from tokyo r50372

r42807 - 2008-12-29 11:16:59 -0800 (Mon, 29 Dec 2008) - dwong - Branch from trunk/sugarcrm r42806 to branches/tokyo/sugarcrm

r32667 - 2008-03-11 17:16:04 -0700 (Tue, 11 Mar 2008) - majed - changes for templating

r23946 - 2007-06-29 18:22:06 -0700 (Fri, 29 Jun 2007) - clee - Provides support for direct PHP file include in Meta-Data structure.

r23498 - 2007-06-08 14:05:19 -0700 (Fri, 08 Jun 2007) - clee - Added support for auto generated tabindex.

r23115 - 2007-05-25 13:07:34 -0700 (Fri, 25 May 2007) - clee - Updated to support PHP file includes.

r22603 - 2007-05-09 13:43:21 -0700 (Wed, 09 May 2007) - clee - Removed warning where there is no include value supplied.

r22571 - 2007-05-08 16:35:35 -0700 (Tue, 08 May 2007) - clee - 

*/


/**
 * Smarty plugin
 * @package Smarty
 * @subpackage plugins
 */


/**
 * Smarty {sugar_include} function plugin
 *
 * Type:     function<br>
 * Name:     sugar_include<br>
 * Purpose:  Handles rendering the global file includes from the metadata files defined
 *           in templateMeta=>includes.
 * 
 * @author Collin Lee {clee@sugarcrm.com}
 * @param array
 * @param Smarty
 */
function smarty_function_sugar_include($params, &$smarty)
{
    global $app_strings;

    if(isset($params['type']) && $params['type'] == 'php') {
		if(!isset($params['file'])) {
		   $smarty->trigger_error($app_strings['ERR_MISSING_REQUIRED_FIELDS'] . 'include');
		} 
		
		$includeFile = $params['file'];
		if(!file_exists($includeFile)) {
		   $smarty->trigger_error($app_strings['ERR_NO_SUCH_FILE'] . ': ' . $includeFile);
		}
		
	    ob_start();
	    require($includeFile);
	    $output_html = ob_get_contents();
	    ob_end_clean();
	    echo $output_html; 
    } else if(isset($params['type']) && $params['type'] == 'smarty') {
        return $smarty->fetch($smarty->loadTemplatePath($params['file']));
	} else if(is_array($params['include'])) {
	   	  $code = '';
	   	  foreach($params['include'] as $include) {
	   	  	      if(isset($include['file'])) {
	   	  	         $file = $include['file'];
	   	  	         if(preg_match('/[\.]js$/si',$file)) {
	   	  	            $code .= "<script src=\"". getJSPath($include['file']) ."\"></script>";
	   	  	         } else if(preg_match('/[\.]php$/si', $file)) {
	   	  	            require_once($file);	
	   	  	         }
	   	  	      } 
	   	  } //foreach
	      return $code;
   	} //if
}
?>