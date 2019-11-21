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

r22745 - 2007-05-11 18:35:10 -0700 (Fri, 11 May 2007) - majed - fixes compiling issue

r22725 - 2007-05-11 16:37:35 -0700 (Fri, 11 May 2007) - clee - Added file.


*/


/**
 * Smarty plugin
 * @package Smarty
 * @subpackage plugins
 */


/**
 * Smarty {sugar_evalcolumn_old} function plugin
 *
 * Type:     function<br>
 * Name:     sugar_evalcolumn_old<br>
 * Purpose:  evaluate a string by substituting values in the rowData parameter. Used for ListViews<br>
 * 
 * @author Wayne Pan {wayne at sugarcrm.com
 * @param array
 * @param Smarty
 */
function smarty_function_sugar_evalcolumn_old($params, &$smarty)
{
    if (!isset($params['var']) || !isset($params['rowData'])) {
        if(!isset($params['var']))  
            $smarty->trigger_error("evalcolumn: missing 'var' parameter");
        if(!isset($params['rowData']))  
            $smarty->trigger_error("evalcolumn: missing 'rowData' parameter");
        return;
    }

    if($params['var'] == '') {
        return;
    }

    global $mod_strings, $app_strings;
    $replace_arr = $params['rowData'];
    if(is_array($mod_strings)) {
        $replace_arr = $replace_arr + array_combine(preg_filter('/^/','MOD.',array_keys($mod_strings)),$mod_strings);
    }
    if(is_array($app_strings)) {
        $replace_arr = $replace_arr + array_combine(preg_filter('/^/','APP.',array_keys($app_strings)),$app_strings);
    }
    if (is_array($params['var'])) {
        foreach ($params['var'] as $key => $value) {
            $params['var'][$key] = searchReplace($value, $replace_arr);
        }
    } else {
        $params['var'] = searchReplace($params['var'], $replace_arr);
    }

    if(isset($params['toJSON'])) {
        $json = getJSONobj();
        $params['var'] = $json->encode($params['var']);
    }
    
    if (!empty($params['assign'])) {
        $smarty->assign($params['assign'], $params['var']);
    } else {
        return $params['var'];
    }
}

function searchReplace($value, &$rowData) {
    preg_match_all('/\{\$(.*)\}/U', $value, $matches);

    for($wp = 0; $wp < count($matches[0]); $wp++) {
        if(isset($rowData[$matches[1][$wp]])) 
            $value = str_replace($matches[0][$wp], $rowData[$matches[1][$wp]], $value);
        else 
            $value = str_replace($matches[0][$wp], '', $value);
    }
    return $value;
}

?>
