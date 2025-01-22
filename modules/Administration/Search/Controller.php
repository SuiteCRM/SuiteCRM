<?php
/**
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

namespace SuiteCRM\Modules\Administration\Search;

use SuiteCRM\Search\SearchConfigurator;
use SuiteCRM\Modules\Administration\Search\MVC\Controller as AbstractController;
use Configurator;
use Exception;
use SuiteCRM\Search\SearchModules;

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}
/**
 * Class Controller handles the the actions for the search settings.
 */
class Controller extends AbstractController
{
    public function __construct()
    {
        parent::__construct(new View());
    }

    /**
     * Saves the configuration from a POST request.
     *
     * If called from ajax it will return a json.
     * @throws Exception
     */
    public function doSave(): void
    {
        $searchEngine = filter_input(INPUT_POST, 'search-engine', FILTER_SANITIZE_STRING);
        $aod = $searchEngine === 'BasicAndAodEngine';

        SearchConfigurator::make()
            ->setEngine($searchEngine)
            ->save();

        SearchModules::saveGlobalSearchSettings();
        $this->doSaveAODConfig($aod);

        if ($this->isAjax()) {
            $this->yieldJson(['status' => 'success']);
        }

        $this->redirect('index.php?module=Administration&action=index');
    }

    /**
     * Saves the configuration getting data from POST.
     * @param bool $enabled
     */
    public function doSaveAODConfig(bool $enabled): void
    {
        $cfg = new Configurator();

        if (!array_key_exists('aod', $cfg->config)) {
            $cfg->config['aod'] = [
                'enable_aod' => '',
            ];
        }

        $cfg->config['aod']['enable_aod'] = $enabled;

        $cfg->saveConfig();
    }
}
