<?php

/*

Modification information for LGPL compliance

r58601 - 2010-10-20 13:52:53 -0700 (Wed, 20 Oct 2010) - engsvnbuild - Author: Stanislav Malyshev <smalyshev@gmail.com>
    handle japanese : too

r58597 - 2010-10-20 11:40:04 -0700 (Wed, 20 Oct 2010) - engsvnbuild - Author: Jenny Gonsalves <jenny@sugarcrm.com>
    Revert "Merge branch 'RC2'"

r58596 - 2010-10-20 09:50:45 -0700 (Wed, 20 Oct 2010) - engsvnbuild - Merge: a813190 e0a061b
Author: Jenny Gonsalves <jenny@sugarcrm.com>
    Merge branch 'RC2'

r56990 - 2010-06-16 13:05:36 -0700 (Wed, 16 Jun 2010) - kjing - snapshot "Mango" svn branch to a new one for GitHub sync

r56989 - 2010-06-16 13:01:33 -0700 (Wed, 16 Jun 2010) - kjing - defunt "Mango" svn dev branch before github cutover

r55980 - 2010-04-19 13:31:28 -0700 (Mon, 19 Apr 2010) - kjing - create Mango (6.1) based on windex

r54045 - 2010-01-26 12:25:05 -0800 (Tue, 26 Jan 2010) - roger - merge from Kobe rev: 53336 - 54021

r51719 - 2009-10-22 10:18:00 -0700 (Thu, 22 Oct 2009) - mitani - Converted to Build 3  tags and updated the build system 

r51634 - 2009-10-19 13:32:22 -0700 (Mon, 19 Oct 2009) - mitani - Windex is the branch for Sugar Sales 1.0 development

r50375 - 2009-08-24 18:07:43 -0700 (Mon, 24 Aug 2009) - dwong - branch kobe2 from tokyo r50372

r42807 - 2008-12-29 11:16:59 -0800 (Mon, 29 Dec 2008) - dwong - Branch from trunk/sugarcrm r42806 to branches/tokyo/sugarcrm

r33667 - 2008-04-01 19:23:40 -0700 (Tue, 01 Apr 2008) - nsingh - bug 20642 fixed.

r29994 - 2007-11-26 12:55:38 -0800 (Mon, 26 Nov 2007) - majed - bug # 18244 translates what is displayed in listviews multiselects

r16020 - 2006-08-16 14:58:47 -0700 (Wed, 16 Aug 2006) - wayne - trim colon by default

r15131 - 2006-07-29 15:46:10 -0700 (Sat, 29 Jul 2006) - majed - translate function


*/


/**
 * Smarty plugin
 * @package Smarty
 * @subpackage plugins
 */


/**
 * Smarty {sugar_translate} function plugin
 *
 * Type:     function<br>
 * Name:     sugar_translate<br>
 * Purpose:  translates a label into the users current language
 *
 * @author Majed Itani {majed at sugarcrm.com
 * @param array
 * @param Smarty
 */
function smarty_function_sugar_translate($params, &$smarty)
{
	if (!isset($params['label'])){
		$smarty->trigger_error("sugar_translate: missing 'label' parameter");
		return '';
	}

	$module = (isset($params['module']))? $params['module']: '';
    if(isset($params['select'])){
    	if(empty($params['select']))
		    $value = "";
		else
		    $value = translate($params['label'] , $module, $params['select']);
	}else{
		$value = translate($params['label'] , $module);
    }
    if (!empty($params['for_js']) && $params['for_js']) {
        if (is_string($value)) {
            $value = addslashes($value);
        }
        $value = str_replace(array('&#039;', '&#39;'), "\'", $value);
    }
    if(isset($params['trimColon']) && !$params['trimColon']) {
        return $value;
    } elseif($params['label'] == '0') {
        return translate("DEFAULT", $module);
    } else {
        return preg_replace("/([:]|\xEF\xBC\x9A)[\\s]*$/", "", $value);
    }
}
