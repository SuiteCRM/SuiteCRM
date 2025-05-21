<?php

/*

Modification information for LGPL compliance

r57851 - 2010-08-20 12:44:11 -0700 (Fri, 20 Aug 2010) - kjing - Author: Jenny Gonsalves <jenny@sugarcrm.com>
    Merging with maint_6_0_1 revisions 57708:57838

r56990 - 2010-06-16 13:05:36 -0700 (Wed, 16 Jun 2010) - kjing - snapshot "Mango" svn branch to a new one for GitHub sync

r56989 - 2010-06-16 13:01:33 -0700 (Wed, 16 Jun 2010) - kjing - defunt "Mango" svn dev branch before github cutover

r56965 - 2010-06-15 10:57:35 -0700 (Tue, 15 Jun 2010) - jenny - Merging with Windex 56827:56958

r55980 - 2010-04-19 13:31:28 -0700 (Mon, 19 Apr 2010) - kjing - create Mango (6.1) based on windex

r52439 - 2009-11-12 17:05:52 -0800 (Thu, 12 Nov 2009) - clee - Updated to allow Rich Text Editor to resize and render HTML content on detailview.

r51719 - 2009-10-22 10:18:00 -0700 (Thu, 22 Oct 2009) - mitani - Converted to Build 3  tags and updated the build system

r51634 - 2009-10-19 13:32:22 -0700 (Mon, 19 Oct 2009) - mitani - Windex is the branch for Sugar Sales 1.0 development

r50375 - 2009-08-24 18:07:43 -0700 (Mon, 24 Aug 2009) - dwong - branch kobe2 from tokyo r50372

r42807 - 2008-12-29 11:16:59 -0800 (Mon, 29 Dec 2008) - dwong - Branch from trunk/sugarcrm r42806 to branches/tokyo/sugarcrm

r30629 - 2007-12-26 08:01:12 -0800 (Wed, 26 Dec 2007) - clee - Changed SugarFieldText.php to automatically set the smarty modifier url2html to be true for DetailViews.  Changed function.sugarvar.php to check for this modifier and render the appropriate call to the smarty modifier.
Modified:
include/SugarFields/Text/SugarFieldText.php
include/Smarty/plugins/function.sugarvar.php

r23083 - 2007-05-24 16:39:44 -0700 (Thu, 24 May 2007) - clee - Code cleanup.

r22459 - 2007-05-02 04:44:56 -0700 (Wed, 02 May 2007) - majed - adds new field types as well as improving meta data driven ui support

r22239 - 2007-04-24 17:22:11 -0700 (Tue, 24 Apr 2007) - clee - Support for nested memberName attribute value (for relate fields)

r22184 - 2007-04-23 17:47:51 -0700 (Mon, 23 Apr 2007) - clee - Latest updates as we continue 5.0 framework development.

r22175 - 2007-04-23 16:43:00 -0700 (Mon, 23 Apr 2007) - clee - Latest updates as we continue 5.0 framework development.

r22125 - 2007-04-20 17:02:26 -0700 (Fri, 20 Apr 2007) - majed - makes it so you don't need to pass in as many variables

r22124 - 2007-04-20 16:54:53 -0700 (Fri, 20 Apr 2007) - clee -

*/


/**
 * Smarty plugin
 * @package Smarty
 * @subpackage plugins
 */


/**
 * Smarty {sugarvar} function plugin
 *
 * Type:     function<br>
 * Name:     sugarvar<br>
 * Purpose:  creates a smarty variable from the parameters
 *
 * @author Wayne Pan {wayne at sugarcrm.com}
 * @param array
 * @param Smarty
 */

function smarty_function_sugarvar($params, &$smarty)
{
	if(empty($params['key']))  {
	    $smarty->trigger_error("sugarvar: missing 'key' parameter");
	    return;
	}

	$object = (empty($params['objectName']))?$smarty->get_template_vars('parentFieldArray'): $params['objectName'];
	$displayParams = $smarty->get_template_vars('displayParams');


	if(empty($params['memberName'])){
		$member = $smarty->tpl_vars['vardef']->value ?? [];
        $member = $member['name'];
    }else{
		$members = explode('.', $params['memberName']);
		$member =  $smarty->get_template_vars($members[0]);
		for($i = 1; $i < count($members); $i++){
			$member = $member[$members[$i]];
		}
	}

    $_contents =  '$'. $object . '.' . $member . '.' . $params['key'];
	if(empty($params['stringFormat']) && empty($params['string'])) {
		$_contents = '{' . $_contents;
		if(!empty($displayParams['htmlescape'])){
			$_contents .= '|escape:\'html\'';
		}
		if(!empty($params['htmlentitydecode'])){
			$_contents .= '|escape:\'html_entity_decode\'';
		}
		if(!empty($displayParams['strip_tags'])){
			$_contents .= '|strip_tags';
		}
		if(!empty($displayParams['url2html'])){
			$_contents .= '|url2html';
		}
		if(!empty($displayParams['nl2br'])){
			$_contents .= '|nl2br';
		}

		$_contents .= '}';
    }
    return $_contents;
}
?>
