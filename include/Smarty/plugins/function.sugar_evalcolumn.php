<?php

/*

Modification information for LGPL compliance

2011-09-23 16:39:38 - 0700 (Fri, 23 Sep 2011) - raagaard - Added support to place the normal field in to a customCode section by replacing @@FIELD@@

2011-08-22 12:52:36 -0700 (Mon, 22 Aug 2011) - jmertic - bug 28321: add support for rendering customCode AND normal field rendering

r56990 - 2010-06-16 13:05:36 -0700 (Wed, 16 Jun 2010) - kjing - snapshot "Mango" svn branch to a new one for GitHub sync

r56989 - 2010-06-16 13:01:33 -0700 (Wed, 16 Jun 2010) - kjing - defunt "Mango" svn dev branch before github cutover

r55980 - 2010-04-19 13:31:28 -0700 (Mon, 19 Apr 2010) - kjing - create Mango (6.1) based on windex

r51719 - 2009-10-22 10:18:00 -0700 (Thu, 22 Oct 2009) - mitani - Converted to Build 3  tags and updated the build system

r51634 - 2009-10-19 13:32:22 -0700 (Mon, 19 Oct 2009) - mitani - Windex is the branch for Sugar Sales 1.0 development

r50375 - 2009-08-24 18:07:43 -0700 (Mon, 24 Aug 2009) - dwong - branch kobe2 from tokyo r50372

r42807 - 2008-12-29 11:16:59 -0800 (Mon, 29 Dec 2008) - dwong - Branch from trunk/sugarcrm r42806 to branches/tokyo/sugarcrm

r42645 - 2008-12-18 13:41:08 -0800 (Thu, 18 Dec 2008) - awu - merging maint_5_2_0 rev41336:HEAD to trunk

r23498 - 2007-06-08 14:05:19 -0700 (Fri, 08 Jun 2007) - clee - Added support for auto generated tabindex.

r22745 - 2007-05-11 18:35:10 -0700 (Fri, 11 May 2007) - majed - fixes compiling issue

r22717 - 2007-05-11 15:02:12 -0700 (Fri, 11 May 2007) - clee - better meta data driven ui support

r22570 - 2007-05-08 16:35:23 -0700 (Tue, 08 May 2007) - clee - Latest revisions.

r22438 - 2007-05-01 18:02:10 -0700 (Tue, 01 May 2007) - clee - Updates for SDUC and developer's build.

r17559 - 2006-11-10 16:01:13 -0800 (Fri, 10 Nov 2006) - wayne - remove extra &

r17518 - 2006-11-07 14:58:53 -0800 (Tue, 07 Nov 2006) - wayne - removed extra &

r17486 - 2006-11-06 15:13:23 -0800 (Mon, 06 Nov 2006) - wayne - added array and tojson support

r17329 - 2006-10-26 15:39:17 -0700 (Thu, 26 Oct 2006) - wayne - defensive code

r12955 - 2006-04-26 18:32:25 -0700 (Wed, 26 Apr 2006) - wayne - custom code in listview smarty stuff


*/


/**
 * Smarty plugin
 * @package Smarty
 * @subpackage plugins
 */


/**
 * Smarty {sugar_evalcolumn} function plugin
 *
 * Type:     function<br>
 * Name:     sugar_evalcolumn<br>
 * Purpose:  evaluate a string by substituting values in the rowData parameter. Used for ListViews<br>
 *
 * @author Wayne Pan {wayne at sugarcrm.com
 * @param array
 * @param Smarty
 */
function smarty_function_sugar_evalcolumn($params, &$smarty)
{
    if (!isset($params['colData']['field']) ) {
        if(empty($params['colData']))
            $smarty->trigger_error("evalcolumn: missing 'colData' parameter");
        if(!isset($params['colData']['field']))
            $smarty->trigger_error("evalcolumn: missing 'colData.field' parameter");
        return;
    }

    if(empty($params['colData']['field'])) {
        return;
    }
    $params['var'] = $params['colData']['field'];
    if(isset($params['toJSON'])) {
        $json = getJSONobj();
        $params['var'] = $json->encode($params['var']);
    }

    if (!empty($params['var']['assign'])) {
        return '{$' . $params['colData']['field']['name'] . '}';
    } else {
    	$code = $params['var']['customCode'];
    	if(isset($params['tabindex']) && preg_match_all("'(<[ ]*?)(textarea|input|select)([^>]*?)(>)'si", $code, $matches, PREG_PATTERN_ORDER)) {
    	   $str_replace = array();
    	   $tabindex = ' tabindex="' . $params['tabindex'] . '" ';
    	   foreach($matches[3] as $match) {
    	   	       $str_replace[$match] = $tabindex . $match;
    	   }
    	   $code = str_replace(array_keys($str_replace), array_values($str_replace), $code);
    	}

        if(isset($params['accesskey']) && preg_match_all("'(<[ ]*?)(textarea|input|select)([^>]*?)(>)'si", $code, $matches, PREG_PATTERN_ORDER)) {
    	   $str_replace = array();
    	   $accesskey = ' accesskey="' . $params['accesskey'] . '" ';
    	   foreach($matches[3] as $match) {
    	   	       $str_replace[$match] = $accesskey . $match;
    	   }
    	   $code = str_replace(array_keys($str_replace), array_values($str_replace), $code);
    	}

        // Add a string replace to swap out @@FIELD@@ for the actual field,
        // we can't do this through customCode directly because the sugar_field smarty function returns smarty code to run on the second pass
        if (!empty($code) && strpos($code,'@@FIELD@@') !== FALSE ) {
            // First we need to fetch extra data about the field
            // sfp == SugarFieldParams
            $sfp = $params;
            $sfp['parentFieldArray'] = 'fields';
            $vardefs = $smarty->get_template_vars('fields');
            $sfp['vardef'] = $vardefs[$params['colData']['field']['name'] ?? ''] ?? [];
            $sfp['displayType'] = 'EditView';
            $sfp['displayParams'] = $params['colData']['field']['displayParams'] ?? [];
            $sfp['typeOverride'] = $params['colData']['field']['type'] ?? '';
            $sfp['formName'] = $smarty->get_template_vars('form_name');

            $fieldText = smarty_function_sugar_field($sfp, $smarty);

            $code = str_replace('@@FIELD@@',$fieldText,$code);
        }

    	//eggsurplus bug 28321: add support for rendering customCode AND normal field rendering
    	if(!empty($params['var']['displayParams']['enableConnectors']) && empty($params['var']['customCodeRenderField'])) {
    	  require_once('include/connectors/utils/ConnectorUtils.php');
    	  $code .= '&nbsp;' . ConnectorUtils::getConnectorButtonScript($params['var']['displayParams'], $smarty);
    	}
    	return $code;
    }


}


?>
