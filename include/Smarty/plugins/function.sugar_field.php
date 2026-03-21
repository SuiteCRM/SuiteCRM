<?php

/*

Modification information for LGPL compliance

r56990 - 2010-06-16 13:05:36 -0700 (Wed, 16 Jun 2010) - kjing - snapshot "Mango" svn branch to a new one for GitHub sync

r56989 - 2010-06-16 13:01:33 -0700 (Wed, 16 Jun 2010) - kjing - defunt "Mango" svn dev branch before github cutover

r55980 - 2010-04-19 13:31:28 -0700 (Mon, 19 Apr 2010) - kjing - create Mango (6.1) based on windex

r53409 - 2010-01-03 19:31:15 -0800 (Sun, 03 Jan 2010) - roger - merge -r50376:HEAD from fuji_newtag_tmp

r51719 - 2009-10-22 10:18:00 -0700 (Thu, 22 Oct 2009) - mitani - Converted to Build 3  tags and updated the build system

r51634 - 2009-10-19 13:32:22 -0700 (Mon, 19 Oct 2009) - mitani - Windex is the branch for Sugar Sales 1.0 development

r50375 - 2009-08-24 18:07:43 -0700 (Mon, 24 Aug 2009) - dwong - branch kobe2 from tokyo r50372

r42807 - 2008-12-29 11:16:59 -0800 (Mon, 29 Dec 2008) - dwong - Branch from trunk/sugarcrm r42806 to branches/tokyo/sugarcrm

r23498 - 2007-06-08 14:05:19 -0700 (Fri, 08 Jun 2007) - clee - Added support for auto generated tabindex.

r22805 - 2007-05-15 13:18:55 -0700 (Tue, 15 May 2007) - roger - RRS: allow for multiple forms per page.

r21721 - 2007-04-11 14:10:04 -0700 (Wed, 11 Apr 2007) - wayne - changed to sugar field handler

r21720 - 2007-04-11 14:07:59 -0700 (Wed, 11 Apr 2007) - wayne - sugar field plugin


*/


/**
 * Smarty plugin
 * @package Smarty
 * @subpackage plugins
 */


/**
 * Smarty {sugar_field} function plugin
 *
 * Type:     function<br>
 * Name:     sugar_field<br>
 * Purpose:  retreives the smarty equivalent for use by TemplateHandler
 *
 * @author Wayne Pan {wayne at sugarcrm.com}
 * @param array
 * @param Smarty
 */
require_once('include/SugarFields/SugarFieldHandler.php');

function smarty_function_sugar_field($params, &$smarty)
{
    if (!isset($params['vardef']) || !isset($params['displayType']) || !isset($params['parentFieldArray'])) {
        if(!isset($params['vardef']))
            $smarty->trigger_error("sugar_field: missing 'vardef' parameter");
        if(!isset($params['displayType']))
            $smarty->trigger_error("sugar_field: missing 'displayType' parameter");
        if(!isset($params['parentFieldArray']))
            $smarty->trigger_error("sugar_field: missing 'parentFieldArray' parameter");

        return;
    }

    static $sfh;
    if(!isset($sfh)) $sfh = new SugarFieldHandler();

    if (!isset($params['displayParams'])){
        $displayParams = array();
    } else {
        $displayParams = $params['displayParams'];
    }

    if (isset($params['labelSpan'])){
        $displayParams['labelSpan'] = $params['labelSpan'];
    }

    if (isset($params['fieldSpan'])){
        $displayParams['fieldSpan'] = $params['fieldSpan'];
    }


    if(!empty($params['typeOverride'])) { // override the type in the vardef?
        $params['vardef']['type'] = $params['typeOverride'];
    }
    if(isset($params['formName'])) $displayParams['formName'] = $params['formName'];

    if(isset($params['field'])) {
        $params['vardef']['name'] = $params['field'];
    }

    if (isset($params['call_back_function'])) {
        $displayParams['call_back_function'] = $params['call_back_function'];
    }

    if(isset($params['skipClearButton'])) {
        $displayParams['skipClearButton'] = $params['skipClearButton'];
    }

    if(isset($params['idName'])) {
        $displayParams['idName'] = $params['idName'];
    }

    if(isset($params['accesskey'])) {
        $displayParams['accesskey'] = $params['accesskey'];
    }

    $_contents = $sfh->displaySmarty($params['parentFieldArray'], $params['vardef'], $params['displayType'], $displayParams, $params['tabindex'] ?? -1);

    return $_contents;
}
?>
