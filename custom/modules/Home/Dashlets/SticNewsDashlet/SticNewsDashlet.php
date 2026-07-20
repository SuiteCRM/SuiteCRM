<?php
/**
 * This file is part of SinergiaCRM.
 * SinergiaCRM is a work developed by SinergiaTIC Association, based on SuiteCRM.
 * Copyright (C) 2013 - 2023 SinergiaTIC Association
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License version 3 as published by the
 * Free Software Foundation.
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
 * You can contact SinergiaTIC Association at email address info@sinergiacrm.org.
 */
if(!defined('sugarEntry') || !sugarEntry) die('Not A Valid Entry Point');

require_once('include/Dashlets/Dashlet.php');

class SticNewsDashlet extends Dashlet { 
    public $displayTpl = 'custom/modules/Home/Dashlets/SticNewsDashlet/display.tpl';
    public $configureTpl = 'custom/modules/Home/Dashlets/SticNewsDashlet/configure.tpl';
    public $defaultURL;
    public $url;

    function __construct($id, $options = null) {
        global $sugar_config, $current_language;
        parent::__construct($id);

        // Gets default user language and defaults to English if not found
        $stic_news_dashlet_url = $sugar_config['stic_news_dashlet_url'];
        $this->defaultURL = !empty($current_language) ? $stic_news_dashlet_url[$current_language] : $stic_news_dashlet_url['en_us'];
        
        $this->isConfigurable = false; // Dashlet won't be configurable
        
        if(empty($options['title'])) { 
            $this->title = translate('LBL_DASHLET_STIC_NEWS', 'Home'); 
        } else {
            $this->title = $options['title'];
        }
        if(empty($options['url'])) { 
            $this->url = $this->defaultURL;
        } else {
            $this->url = $options['url'];
        }

        if(empty($options['height']) || (int)$options['height'] < 1 ) { 
            $this->height = 315; 
        } else {
            $this->height = (int)$options['height'];
        }

        if(isset($options['autoRefresh'])) $this->autoRefresh = $options['autoRefresh'];
    }

    /**
     * This function isn't used cause the dashlet is not configurable. Anyway, it is maintained here in case of future needs.
     */
    function displayOptions() {	
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
        if($this->isAutoRefreshable()) {	
       		$ss->assign('isRefreshable', true);	
			$ss->assign('autoRefresh', $GLOBALS['app_strings']['LBL_DASHLET_CONFIGURE_AUTOREFRESH']);	
			$ss->assign('autoRefreshOptions', $this->getAutoRefreshOptions());	
			$ss->assign('autoRefreshSelect', $this->autoRefresh);	
		}	

        return  $ss->fetch('custom/modules/Home/Dashlets/SticNewsDashlet/configure.tpl');        	
    }	

    /**
     * This function isn't used cause the dashlet is not configurable. Anyway, it is maintained here in case of future needs.
     */
    function saveOptions($req) {	
        $options = array();	

        if ( isset($req['title']) ) {	
            $options['title'] = $req['title'];	
        }	
        if ( isset($req['url']) ) {	
            $options['url'] = $req['url'];	
        }	
        if ( isset($req['height']) ) {	
            $options['height'] = (int)$req['height'];	
        }	
        $options['autoRefresh'] = empty($req['autoRefresh']) ? '0' : $req['autoRefresh'];	

        return $options;	
    }

    function display(){

        $timestamp = time();

        return parent::display() . "
        <iframe 
            class='teamNoticeBox'
            title='{$this->title}'
            src='{$this->url}?{$timestamp}'
            height='{$this->height}px'>
        </iframe>";
    }
}
