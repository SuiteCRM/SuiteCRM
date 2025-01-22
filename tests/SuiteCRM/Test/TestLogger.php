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

namespace SuiteCRM\Test;

/**
 * Class TestLogger
 * @package SuiteCRM\Tests\SuiteCRM\Test
 */
#[\AllowDynamicProperties]
class TestLogger
{

    /**
     * @var array
     */
    public $calls;

    /**
     * @var array
     */
    public $notes;

    /**
     * TestLogger constructor.
     */
    public function __construct()
    {
        $this->reset();
    }

    /**
     * @param $name
     * @param $arguments
     */
    public function __call($name, $arguments)
    {
        $this->calls[$name][] = $arguments;
        $this->notes[] = [
            'entry_date' => date('Y-m-d H:i:s'),
            'level' => $name,
            'message' => array_shift($arguments),
            'arguments' => $arguments,
            'backtrace' => debug_backtrace(),
        ];
    }

    /**
     * @param string|string[] $levels
     * @return array
     */
    public function getNotes($levels = 'fatal'): array
    {
        $results = [];
        if (is_string($levels)) {
            $levels = explode(',', $levels);
            foreach ($levels as &$level) {
                $level = strtolower(trim($level));
            }
        }
        foreach ($this->notes as $note) {
            if (in_array($note['level'], $levels, true)) {
                $results[] = $note;
            }
        }

        return $results;
    }

    public function reset(): void
    {
        $this->calls = [];
        $this->notes = [];
    }
}
