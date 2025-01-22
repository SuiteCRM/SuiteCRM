<?php
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}
/**
 *
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
 *
 * SuiteCRM is an extension to SugarCRM Community Edition developed by SalesAgility Ltd.
 * Copyright (C) 2011 - 2018 SalesAgility Ltd.
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License version 3 as published by the
 * Free Software Foundation with the addition of the following permission added
 * to Section 15 as permitted in Section 7(a): FOR ANY PART OF THE COVERED WORK
 * IN WHICH THE COPYRIGHT IS OWNED BY SUGARCRM, SUGARCRM DISCLAIMS THE WARRANTY
 * OF NON INFRINGEMENT OF THIRD PARTY RIGHTS.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
 * details.
 *
 * You should have received a copy of the GNU Affero General Public License along with
 * this program; if not, see http://www.gnu.org/licenses or write to the Free
 * Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 *
 * You can contact SugarCRM, Inc. headquarters at 10050 North Wolfe Road,
 * SW2-130, Cupertino, CA 95014, USA. or at email address contact@sugarcrm.com.
 *
 * The interactive user interfaces in modified source and object code versions
 * of this program must display Appropriate Legal Notices, as required under
 * Section 5 of the GNU Affero General Public License version 3.
 *
 * In accordance with Section 7(b) of the GNU Affero General Public License version 3,
 * these Appropriate Legal Notices must retain the display of the "Powered by
 * SugarCRM" logo and "Supercharged by SuiteCRM" logo. If the display of the logos is not
 * reasonably feasible for technical reasons, the Appropriate Legal Notices must
 * display the words "Powered by SugarCRM" and "Supercharged by SuiteCRM".
 */


require_once('include/Dashlets/Dashlet.php');


#[\AllowDynamicProperties]
class iFrameDashlet extends Dashlet
{
    public $displayTpl = 'modules/Home/Dashlets/iFrameDashlet/display.tpl';
    public $configureTpl = 'modules/Home/Dashlets/iFrameDashlet/configure.tpl';
    public $defaultURL = 'http://apps.sugarcrm.com/dashlet/sugarcrm-news-dashlet.html?lang=@@LANG@@&edition=@@EDITION@@&ver=@@VER@@';
    public $url;
    protected $allowed_schemes = array("http", "https");

    public function __construct($id, $options = null)
    {
        parent::__construct($id);
        $this->isConfigurable = true;

        if (empty($this->title)) {
            $this->title = translate('LBL_DASHLET_TITLE', 'Home');
            $this->title = translate('LBL_DASHLET_DISCOVER_SUITE', 'Home');
        }

        if (!empty($options['titleLabel'])) {
            $this->title = translate($options['titleLabel'], 'Home');
        } elseif (!empty($options['title'])) {
            $this->title = $options['title'];
        }

        if (empty($options['url'])) {
            $this->url = $this->defaultURL;
            $this->url = 'https://suitecrm.com/';
        } else {
            $this->url = $options['url'];
        }

        if (isSelfRequest($this->url)) {
            $this->url = '';
        }

        if (empty($options['height']) || (int)$options['height'] < 1) {
            $this->height = 315;
        } else {
            $this->height = (int)$options['height'];
        }

        if (isset($options['autoRefresh'])) {
            $this->autoRefresh = $options['autoRefresh'];
        }
    }

    protected function checkURL()
    {
        $scheme = parse_url((string) $this->url, PHP_URL_SCHEME);
        if (!in_array($scheme, $this->allowed_schemes)) {
            $this->url = 'about:blank';
            return false;
        }
        return true;
    }

    public function displayOptions()
    {
        global $app_strings;
        $ss = new Sugar_Smarty();
        $ss->assign('titleLBL', translate('LBL_DASHLET_OPT_TITLE', 'Home'));
        $ss->assign('urlLBL', translate('LBL_DASHLET_OPT_URL', 'Home'));
        $ss->assign('heightLBL', translate('LBL_DASHLET_OPT_HEIGHT', 'Home'));
        $ss->assign('title', $this->title);
        $ss->assign('url', $this->url);
        $ss->assign('id', $this->id);
        $ss->assign('height', $this->height);
        $ss->assign('saveLBL', $app_strings['LBL_SAVE_BUTTON_LABEL']);
        $ss->assign('clearLBL', $app_strings['LBL_CLEAR_BUTTON_LABEL']);
        if ($this->isAutoRefreshable()) {
            $ss->assign('isRefreshable', true);
            $ss->assign('autoRefresh', $GLOBALS['app_strings']['LBL_DASHLET_CONFIGURE_AUTOREFRESH']);
            $ss->assign('autoRefreshOptions', $this->getAutoRefreshOptions());
            $ss->assign('autoRefreshSelect', $this->autoRefresh);
        }

        return  $ss->fetch($this->configureTpl);
    }

    public function saveOptions($req)
    {
        $options = array();

        if (isset($req['title'])) {
            $options['title'] = $req['title'];
        }
        if (isset($req['url'])) {
            $options['url'] = $req['url'];
        }
        if (isset($req['height'])) {
            $options['height'] = (int)$req['height'];
        }
        $options['autoRefresh'] = empty($req['autoRefresh']) ? '0' : $req['autoRefresh'];

        return $options;
    }

    public function display()
    {
        $sugar_edition = 'COM';


        $out_url = str_replace(
            array('@@LANG@@','@@VER@@','@@EDITION@@'),
            array($GLOBALS['current_language'],$GLOBALS['sugar_config']['sugar_version'],$sugar_edition),
            (string) $this->url
        );
        $title = $this->title;
        if (empty($title)) {
            $title = 'empty';
        }

        $result = parent::display();
        if ($this->checkURL()) {
            $result .= "<iframe class='teamNoticeBox' title='{$title}' src='{$out_url}' height='{$this->height}px'></iframe>";
        } else {
            $result .= '<table cellpadding="0" cellspacing="0" width="100%" border="0" class="list view"><tr height="20"><td colspan="11"><em>' . translate('LBL_DASHLET_INCORRECT_URL', 'Home') . '</em></td></tr></table>';
        }
        return $result;
    }
}
