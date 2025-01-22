<?php

/*

Modification information for LGPL compliance

r56990 - 2010-06-16 13:05:36 -0700 (Wed, 16 Jun 2010) - kjing - snapshot "Mango" svn branch to a new one for GitHub sync

r56989 - 2010-06-16 13:01:33 -0700 (Wed, 16 Jun 2010) - kjing - defunt "Mango" svn dev branch before github cutover

r55980 - 2010-04-19 13:31:28 -0700 (Mon, 19 Apr 2010) - kjing - create Mango (6.1) based on windex

r51719 - 2009-10-22 10:18:00 -0700 (Thu, 22 Oct 2009) - mitani - Converted to Build 3  tags and updated the build system

r51634 - 2009-10-19 13:32:22 -0700 (Mon, 19 Oct 2009) - mitani - Windex is the branch for Sugar Sales 1.0 development

r50375 - 2009-08-24 18:07:43 -0700 (Mon, 24 Aug 2009) - dwong - branch kobe2 from tokyo r50372

r45763 - 2009-04-01 12:16:18 -0700 (Wed, 01 Apr 2009) - majed - Removed half of the require_once and include_onces in the product that were redundant or could be handled easily by the auto loader

r42807 - 2008-12-29 11:16:59 -0800 (Mon, 29 Dec 2008) - dwong - Branch from trunk/sugarcrm r42806 to branches/tokyo/sugarcrm

r33741 - 2008-04-03 11:03:29 -0700 (Thu, 03 Apr 2008) - bsoufflet - Change the smarty function : sugar_currency_format
Now you can specify decimals, round, convert, currency_symbol parameters in the listview defs.

r33739 - 2008-04-03 10:58:52 -0700 (Thu, 03 Apr 2008) - bsoufflet - Change the smarty function : sugar

r28687 - 2007-10-22 17:16:54 -0700 (Mon, 22 Oct 2007) - jenny - Bug 16501: Checking in Majed's changes...

r26209 - 2007-08-28 01:19:52 -0700 (Tue, 28 Aug 2007) - clee - Fixed issue with currency symbol formatting for subtotal and line items.

r25610 - 2007-08-14 00:34:06 -0700 (Tue, 14 Aug 2007) - clee - Cleaned the code up.  Removed all the commented blocks and optimized to not include files unless needed.

r22865 - 2007-05-16 14:42:03 -0700 (Wed, 16 May 2007) - clee - Updated to user currency_format_number

r15484 - 2006-08-03 15:58:50 -0700 (Thu, 03 Aug 2006) - chris - Currency Display fixes for i18n deliverables

r14851 - 2006-07-20 17:16:45 -0700 (Thu, 20 Jul 2006) - wayne - return nothing if nothing passed in

r14718 - 2006-07-17 17:39:10 -0700 (Mon, 17 Jul 2006) - wayne - format the currency


*/


/**
 * Smarty plugin
 * @package Smarty
 * @subpackage plugins
 */


/**
 * Smarty {sugar_currency_format} function plugin
 *
 * Type:     function<br>
 * Name:     sugar_currency_format<br>
 * Purpose:  formats a number
 *
 * @author Wayne Pan {wayne at sugarcrm.com}
 * @param array
 * @param Smarty
 */
function smarty_function_sugar_currency_format($params, &$smarty) {

    // Bug #47406 : Currency field doesn't accept 0.00 as default value
	if(!isset($params['var']) || $params['var'] === '') {
        return '';
    }

    global $locale;
    if(empty($params['currency_id'])){
        if ($_REQUEST['action'] === 'Popup' && $params['field_name'] !== 'amount_usdollar') {
            $params['currency_id'] = getCurrencyId($_REQUEST['module'], $params['id']);
            $params['currency_symbol'] = $locale->currencies[$params['currency_id']]['symbol'];
        } else {
            $params['currency_id'] = $locale->getPrecedentPreference('currency');
            if(!isset($params['convert'])) {
                $params['convert'] = true;
            }

            if(!isset($params['currency_symbol'])) {
                $params['currency_symbol'] = $locale->getPrecedentPreference('default_currency_symbol');
            }
        }
    }

    $_contents = currency_format_number($params['var'], $params);

    if (!empty($params['assign'])) {
        $smarty->assign($params['assign'], $_contents);
    } else {
        return $_contents;
    }
}

?>
