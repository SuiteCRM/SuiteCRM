<?php
/**
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
 *
 * SuiteCRM is an extension to SugarCRM Community Edition developed by SalesAgility Ltd.
 * Copyright (C) 2011 - 2021 SalesAgility Ltd.
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

namespace SuiteCRM\Search\UI;

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

use InvalidArgumentException;
use LoggerManager;
use SuiteCRM\Search\SearchWrapper;
use SuiteCRM\Search\UI\MVC\View;
use SuiteCRM\Utility\StringUtils;

/**
 * Class SearchFormView handles the search bar and form.
 */
class SearchFormView extends View
{
    public function __construct()
    {
        parent::__construct(__DIR__ . '/templates/search.form.tpl');
    }

    /** @inheritdoc */
    public function display(): void
    {
        global $sugar_config, $app_strings;

        $sizes = $this->makeSizesFromConfig();
        $engines = [];

        foreach (SearchWrapper::getEngines() as $engine) {
            $engines[$engine] = StringUtils::camelToTranslation($engine);
        }

        if ($sugar_config['search']['ElasticSearch']['enabled'] === false) {
            unset($engines['ElasticSearchEngine']);
        }

        $currentEngine = SearchWrapper::getDefaultEngine();

        if ($currentEngine === 'BasicSearchEngine' || $currentEngine === 'ElasticSearchEngine') {
            $engines = [];
        }

        $this->smarty->assign('sizeOptions', $sizes);
        $this->smarty->assign('engineOptions', $engines);
        $this->smarty->assign('APP', $app_strings);

        parent::display();
    }

    /**
     * Makes an array with the page size from the sugar config.
     *
     * @return mixed[]|null
     */
    protected function makeSizesFromConfig(): ?array
    {
        global $sugar_config;

        if (!isset($sugar_config['search']['pagination']['min'])) {
            LoggerManager::getLogger()->warn('Configuration does not contains value for search pagination min');
        }

        if (!isset($sugar_config['search']['pagination']['step'])) {
            LoggerManager::getLogger()->warn('Configuration does not contains value for search pagination step');
        }

        if (!isset($sugar_config['search']['pagination']['max'])) {
            LoggerManager::getLogger()->warn('Configuration does not contains value for search pagination max');
        }

        $min = $sugar_config['search']['pagination']['min'] ?? null;
        $step = $sugar_config['search']['pagination']['step'] ?? null;
        $max = $sugar_config['search']['pagination']['max'] ?? null;

        try {
            return $this->makeSizes($min, $step, $max);
        } catch (InvalidArgumentException $exception) {
            return $this->makeSizes(10, 10, 50);
        }
    }

    /**
     * Makes an array with the page size from the given parameters.
     *
     * @param int|null $min
     * @param int|null $step
     * @param int|null $max
     *
     * @return array
     * @throws InvalidArgumentException in case of failure
     */
    protected function makeSizes(?int $min, ?int $step, ?int $max): array
    {
        $min = (int)$min;
        $step = (int)$step;
        $max = (int)$max;

        if (!is_int($min) || !is_int($step) || !is_int($max)) {
            throw new InvalidArgumentException('Arguments must be integers');
        }

        if ($max === 0 || $step === 0 || $min === 0) {
            throw new InvalidArgumentException('Arguments cannot be zero');
        }

        if ($min > $max) {
            throw new InvalidArgumentException('$min must be smaller than $max');
        }

        $sizes = [];

        for ($it = $min; $it <= $max; $it += $step) {
            $sizes[$it] = $it;
        }

        return $sizes;
    }
}
