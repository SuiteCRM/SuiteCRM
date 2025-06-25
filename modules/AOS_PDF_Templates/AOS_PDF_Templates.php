<?php
/**
 * Products, Quotations & Invoices modules.
 * Extensions to SugarCRM
 * @package Advanced OpenSales for SugarCRM
 * @subpackage Products
 * @copyright SalesAgility Ltd http://www.salesagility.com
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU AFFERO GENERAL PUBLIC LICENSE as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU AFFERO GENERAL PUBLIC LICENSE
 * along with this program; if not, see http://www.gnu.org/licenses
 * or write to the Free Software Foundation,Inc., 51 Franklin Street,
 * Fifth Floor, Boston, MA 02110-1301  USA
 *
 * @author SalesAgility Ltd <support@salesagility.com>
 */

/**
 * THIS CLASS IS FOR DEVELOPERS TO MAKE CUSTOMIZATIONS IN
 */
require_once('modules/AOS_PDF_Templates/AOS_PDF_Templates_sugar.php');
#[\AllowDynamicProperties]
class AOS_PDF_Templates extends AOS_PDF_Templates_sugar
{
    public function __construct()
    {
        parent::__construct();
        // STIC-Custom 20220124 MHP - Set in pdf_template_type_dom an ordered list with the names of the modules displayed in the menu tabs in the current user's language  
        // STIC#564   
        global $app_list_strings;
        $app_list_strings['pdf_template_type_dom'] = static::loadTabModules();
        // END STIC-Custom        
    }


    /**
     * @deprecated deprecated since version 7.6, PHP4 Style Constructors are deprecated and will be remove in 7.8, please update your code, use __construct instead
     */
    public function AOS_PDF_Templates()
    {
        $deprecatedMessage = 'PHP4 Style Constructors are deprecated and will be remove in 7.8, please update your code';
        if (isset($GLOBALS['log'])) {
            $GLOBALS['log']->deprecated($deprecatedMessage);
        } else {
            trigger_error($deprecatedMessage, E_USER_DEPRECATED);
        }
        self::__construct();
    }

    // STIC-Custom 20220124 MHP - Create loadTabModules function
    // STIC#564  
    /**
     * Returns an ordered list with the names of the modules displayed in the menu tabs in the current user's language
     * @return array
     */
    public static function loadTabModules()
    {
        global $app_list_strings;
        // STIC-Custom 20250128 ART - Tracker Module
        // https://github.com/SinergiaTIC/SinergiaCRM/pull/211
        global $current_user;
        // END STIC-Custom
        include_once 'modules/MySettings/TabController.php';
        $controller = new TabController();
        $currentTabs = $controller->get_system_tabs();

        // Modules to be excluded
        $excludedModules = ['Home', 'Calendar'];

        $modules = array();
        foreach($currentTabs as $key => $mod){
            if (!in_array($mod, $excludedModules)) {
                $modules[$key] = (isset($app_list_strings['moduleList'][$key])) ? $app_list_strings['moduleList'][$key] : $key;
            }
        }
        // STIC-Custom 20250128 ART - Tracker Module
        // https://github.com/SinergiaTIC/SinergiaCRM/pull/211
        // Generate pdf document of the Trackers module if the user is admin
        if ($current_user->is_admin) {
            // Include 'Trackers' module in the list
            $modules['Trackers'] = $app_list_strings['moduleList']['Trackers'];
        }
        // END STIC-Custom

        asort($modules);
        return $modules;
    }
    // END STIC-Custom    
     
    public function cleanBean()
    {
        parent::cleanBean();
        $this->pdfheader = purify_html($this->pdfheader, ['HTML.ForbiddenElements' => ['iframe' => true]]);
        $this->description = purify_html($this->description, ['HTML.ForbiddenElements' => ['iframe' => true]]);
        $this->pdffooter = purify_html($this->pdffooter, ['HTML.ForbiddenElements' => ['iframe' => true]]);
    }
}
