<?php

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

/**
 * Class FormulaNode
 */
#[\AllowDynamicProperties]
class FormulaNode
{
    public $text;
    public $level;
    public $parent;
    public $children = array();
    public $evaluatedValue;

    public function __construct($text, $level, $parent = null)
    {
        $this->text = $text;
        $this->level = $level;
        $this->parent = $parent;
    }

    public function addChild($childNode)
    {
        $this->children [] = $childNode;
    }

    public function isLeaf()
    {
        return count($this->children) === 0;
    }
}

/**
 * Class FormulaCalculator
 */
#[\AllowDynamicProperties]
class FormulaCalculator
{
    public const START_TERMINAL = "{";
    public const END_TERMINAL = "}";
    public const PARAMETER_SEPARATOR_TERMINAL = ";";
    public const CONFIGURATOR_NAME = "SweeterCalc";

    private $parameters;
    private $relationParameters;
    private $currentModule;
    private $creatorUserId;
    private $configurator;
    private $debugEnabled;
    private $debugFileName;

    /**
     * FormulaCalculator constructor.
     *
     * @param $parameters
     * @param $relationParameters
     * @param $currentModule
     * @param $creatorUserId
     */
    public function __construct($parameters, $relationParameters, $currentModule, $creatorUserId)
    {
        $this->parameters = $parameters;
        $this->relationParameters = $relationParameters;
        $this->currentModule = $currentModule;
        $this->creatorUserId = $creatorUserId;

        require_once 'modules/Configurator/Configurator.php';
        $this->configurator = new Configurator();
        $this->configurator->loadConfig();

        $configName = $this->configurator->config[FormulaCalculator::CONFIGURATOR_NAME] ?? '';

        $debugEnabled = $configName['DebugEnabled'] ?? 0;
        $this->debugEnabled = $debugEnabled == 1;
        $this->debugFileName = $configName['DebugFileName'] ?? 'SweeterCalc.log';
    }

    /**
     * @param $formula
     *
     * @return mixed|string
     * @throws Exception
     */
    public function calculateFormula($formula)
    {
        try {
            $currentEncoding = mb_internal_encoding();

            mb_internal_encoding("UTF-8");

            $this->log("--------------------------------------------------------------------------------------------------------");
            $this->log("Evaluating expression: '$formula'.");

            $rootNode = $this->createTree($formula);
            $evaluated = $this->evaluateTreeLevel($rootNode);

            $this->log("Expression evaluated, value is: '$evaluated'.");
            $this->log("--------------------------------------------------------------------------------------------------------");

            mb_internal_encoding($currentEncoding);

            return $evaluated;
        } catch (Exception $e) {
            $this->log('Exception: ' . $e->getMessage());
            throw $e;
        }
    }

    /**
     * @param $content
     */
    private function log($content)
    {
        if (!$this->debugEnabled) {
            return;
        }

        $currentContent = file_exists($this->debugFileName) ? file_get_contents($this->debugFileName) : "";
        file_put_contents($this->debugFileName, $currentContent . "[" . date("Y-m-d H:i:s") . "] " . $content . "\n");
    }

    /**
     * @param $content
     *
     * @return FormulaNode
     */
    private function createTree($content)
    {
        $rootNode = new FormulaNode($content, 0);

        $this->findLexicalElementsOnLevel($content, 0, $rootNode);

        return $rootNode;
    }

    /**
     * @param $content
     * @param $level
     * @param $node
     */
    private function findLexicalElementsOnLevel($content, $level, &$node)
    {
        $characters = preg_split('//u', (string) $content, -1, PREG_SPLIT_NO_EMPTY);
        $terminalLevel = 0;
        $hasChild = false;

        $currentText = "";
        $charactersCount = is_countable($characters) ? count($characters) : 0;
        for ($i = 0; $i < $charactersCount; $i++) {
            $char = $characters[$i];

            if ($terminalLevel > 0) {
                $currentText .= $char;
            }

            if ($char === FormulaCalculator::START_TERMINAL) {
                if ($terminalLevel == 0) {
                    $currentText .= $char;
                }

                $terminalLevel++;
            } elseif ($char === FormulaCalculator::END_TERMINAL) {
                $terminalLevel--;

                if ($terminalLevel == 0) {
                    $newLevel = $level + 1;
                    $newNode = new FormulaNode($currentText, $newLevel, $node);
                    $node->addChild($newNode);

                    $this->findLexicalElementsOnLevel(mb_substr($currentText, 1, -1), $newLevel, $newNode);

                    $currentText = "";
                    $hasChild = true;
                }
            }
        }
    }

    /**
     * @param $node
     *
     * @return int|mixed|string
     */
    private function evaluateTreeLevel(&$node)
    {
        if ($node->isLeaf()) {
            $node->evaluatedValue = $this->evaluateNode($node->text);
            $this->log("Node value: " . $node->evaluatedValue);
            $node->evaluatedValue = $this->evaluateLeaf($node->evaluatedValue);

            $this->log("Evaluating leaf: " . $node->text);
            $this->log("Leaf value: " . $node->evaluatedValue);

            return $node->evaluatedValue;
        }

        $childItems = array();
        foreach ($node->children as $child) {
            $childItems [] = array(
                'value' => $child->text,
                'evaluatedValue' => $this->evaluateTreeLevel($child),
            );
        }

        if ($node->level > 0) {
            $node->evaluatedValue = $this->evaluateNode($node->text, $childItems);
            $this->log("Node value: " . $node->evaluatedValue);
        } else {
            $evaluatedValue = $node->text;

            foreach ($childItems as $childItem) {
                $pos = strpos((string) $evaluatedValue, (string) $childItem['value']);
                if ($pos !== false) {
                    $this->log("Going to replace child value '" . $childItem['value'] . "' in expression: " . $evaluatedValue);
                    $evaluatedValue = substr_replace(
                        $evaluatedValue,
                        $childItem['evaluatedValue'],
                        $pos,
                        strlen((string) $childItem['value'])
                    );
                    $this->log("Replaced child value '" . $childItem['evaluatedValue'] . "'. New expression: " . $evaluatedValue);
                }
            }

            $node->evaluatedValue = $evaluatedValue;
        }

        return $node->evaluatedValue;
    }

    /**
     * @param $text
     * @param array $childItems
     *
     * @return string
     */
    private function evaluateNode($text, $childItems = array())
    {
        if (count($childItems) == 0) {
            $this->log("Evaluating node: " . $text . " with no children.");
        } else {
            $this->log("Evaluating node: " . $text . " with children: ");
            $this->logVardump($childItems);
        }

        // Logical functions
        if (($params = $this->evaluateFunctionParams("equal", $text, $childItems)) != null) {
            return $params[0] == $params[1] ? "1" : "0";
        }

        if (($params = $this->evaluateFunctionParams("notEqual", $text, $childItems)) != null) {
            return $params[0] != $params[1] ? "1" : "0";
        }

        if (($params = $this->evaluateFunctionParams("greaterThan", $text, $childItems)) != null) {
            return $params[0] > $params[1] ? "1" : "0";
        }

        if (($params = $this->evaluateFunctionParams("greaterThanOrEqual", $text, $childItems)) != null) {
            return $params[0] >= $params[1] ? "1" : "0";
        }

        if (($params = $this->evaluateFunctionParams("lessThan", $text, $childItems)) != null) {
            return $params[0] < $params[1] ? "1" : "0";
        }

        if (($params = $this->evaluateFunctionParams("lessThanOrEqual", $text, $childItems)) != null) {
            return $params[0] <= $params[1] ? "1" : "0";
        }

        if (($params = $this->evaluateFunctionParams("empty", $text, $childItems)) != null) {
            return $params[0] == "" ? "1" : "0";
        }

        if (($params = $this->evaluateFunctionParams("notEmpty", $text, $childItems)) != null) {
            return $params[0] != "" ? "1" : "0";
        }

        if (($params = $this->evaluateFunctionParams("not", $text, $childItems)) != null) {
            return $params[0] == "0" ? "1" : "0";
        }

        if (($params = $this->evaluateFunctionParams("and", $text, $childItems)) != null) {
            return ($params[0] && $params[1]) ? "1" : "0";
        }

        if (($params = $this->evaluateFunctionParams("or", $text, $childItems)) != null) {
            return ($params[0] || $params[1]) ? "1" : "0";
        }

        // Control functions
        if (($params = $this->evaluateFunctionParams("ifThenElse", $text, $childItems)) != null) {
            return $params[0] ? $params[1] : $params[2];
        }

        // String functions
        if (($params = $this->evaluateFunctionParams("substring", $text, $childItems)) != null) {
            // Workaround for PHP < 5.4.8
            if (isset($params[2])) {
                return mb_substr($params[0], (int)$params[1], (int)$params[2]);
            }
            return mb_substr($params[0], (int)$params[1]);
        }

        if (($params = $this->evaluateFunctionParams("length", $text, $childItems)) != null) {
            return mb_strlen($params[0]);
        }

        if (($params = $this->evaluateFunctionParams("replace", $text, $childItems)) != null) {
            return str_replace($params[0], $params[1], (string) $params[2]);
        }

        if (($params = $this->evaluateFunctionParams("position", $text, $childItems)) != null) {
            $pos = mb_strpos((string) $params[0], (string) $params[1]);

            return ($pos == false) ? -1 : $pos;
        }

        if (($params = $this->evaluateFunctionParams("lowercase", $text, $childItems)) != null) {
            return mb_strtolower($params[0]);
        }

        if (($params = $this->evaluateFunctionParams("uppercase", $text, $childItems)) != null) {
            return mb_strtoupper($params[0]);
        }

        // Mathematical calculations
        if (($params = $this->evaluateFunctionParams("add", $text, $childItems)) != null) {
            return $this->parseFloat($params[0]) + $this->parseFloat($params[1]);
        }

        if (($params = $this->evaluateFunctionParams("subtract", $text, $childItems)) != null) {
            return $this->parseFloat($params[0]) - $this->parseFloat($params[1]);
        }

        if (($params = $this->evaluateFunctionParams("multiply", $text, $childItems)) != null) {
            return $this->parseFloat($params[0]) * $this->parseFloat($params[1]);
        }

        if (($params = $this->evaluateFunctionParams("divide", $text, $childItems)) != null) {
            return $this->parseFloat($params[0]) / $this->parseFloat($params[1]);
        }

        if (($params = $this->evaluateFunctionParams("power", $text, $childItems)) != null) {
            return pow($this->parseFloat($params[0]), $this->parseFloat($params[1]));
        }

        if (($params = $this->evaluateFunctionParams("squareRoot", $text, $childItems)) != null) {
            return sqrt($this->parseFloat($params[0]));
        }

        if (($params = $this->evaluateFunctionParams("absolute", $text, $childItems)) != null) {
            return abs($this->parseFloat($params[0]));
        }

        // Date functions
        if (($params = $this->evaluateFunctionParams("now", $text, $childItems)) != null) {
            return date($params[0]);
        }

        if (($params = $this->evaluateFunctionParams("yesterday", $text, $childItems)) != null) {
            return date($params[0], time() - 60 * 60 * 24);
        }

        if (($params = $this->evaluateFunctionParams("tomorrow", $text, $childItems)) != null) {
            return date($params[0], time() + 60 * 60 * 24);
        }

        if (($params = $this->evaluateFunctionParams("date", $text, $childItems)) != null) {
            return date($params[0], strtotime($params[1]));
        }

        if (($params = $this->evaluateFunctionParams("datediff", $text, $childItems)) != null) {
            $d1 = new DateTime($this->getDBFormat($params[0]));
            $d2 = new DateTime($this->getDBFormat($params[1]));
            $diff = $d1->diff($d2);

            switch ($params[2]) {
                case 'years':
                    return $diff->y;
                case 'months':
                    return $diff->y * 12 + $diff->m;
                case 'days':
                    return $diff->days;
                case 'hours':
                    return $diff->days * 24 + $diff->h;
                case 'minutes':
                    return ($diff->days * 24 + $diff->h) * 60 + $diff->i;
                case 'seconds':
                    return (($diff->days * 24 + $diff->h) * 60 + $diff->i) * 60 + $diff->s;
                default:
                    return $diff->days;
            }
        }

        if (($params = $this->evaluateFunctionParams("addYears", $text, $childItems)) != null) {
            return $this->modifyDate($params[0], $params[1], $params[2], 'Y');
        }

        if (($params = $this->evaluateFunctionParams("addMonths", $text, $childItems)) != null) {
            return $this->modifyDate($params[0], $params[1], $params[2], 'M');
        }

        if (($params = $this->evaluateFunctionParams("addDays", $text, $childItems)) != null) {
            return $this->modifyDate($params[0], $params[1], $params[2], 'D');
        }

        if (($params = $this->evaluateFunctionParams("addHours", $text, $childItems)) != null) {
            return $this->modifyDate($params[0], $params[1], $params[2], 'H', true);
        }

        if (($params = $this->evaluateFunctionParams("addMinutes", $text, $childItems)) != null) {
            return $this->modifyDate($params[0], $params[1], $params[2], 'M', true);
        }

        if (($params = $this->evaluateFunctionParams("addSeconds", $text, $childItems)) != null) {
            return $this->modifyDate($params[0], $params[1], $params[2], 'S', true);
        }

        if (($params = $this->evaluateFunctionParams("subtractYears", $text, $childItems)) != null) {
            return $this->modifyDate($params[0], $params[1], $params[2], 'Y', false, false);
        }

        if (($params = $this->evaluateFunctionParams("subtractMonths", $text, $childItems)) != null) {
            return $this->modifyDate($params[0], $params[1], $params[2], 'M', false, false);
        }

        if (($params = $this->evaluateFunctionParams("subtractDays", $text, $childItems)) != null) {
            return $this->modifyDate($params[0], $params[1], $params[2], 'D', false, false);
        }

        if (($params = $this->evaluateFunctionParams("subtractHours", $text, $childItems)) != null) {
            return $this->modifyDate($params[0], $params[1], $params[2], 'H', true, false);
        }

        if (($params = $this->evaluateFunctionParams("subtractMinutes", $text, $childItems)) != null) {
            return $this->modifyDate($params[0], $params[1], $params[2], 'M', true, false);
        }

        if (($params = $this->evaluateFunctionParams("subtractSeconds", $text, $childItems)) != null) {
            return $this->modifyDate($params[0], $params[1], $params[2], 'S', true, false);
        }

        return $text;
    }

    /**
     * @param $obj
     */
    private function logVardump($obj)
    {
        if (!$this->debugEnabled) {
            return;
        }

        ob_start();
        var_dump($obj);
        $str = ob_get_contents();
        ob_end_clean();

        $this->log($str);
    }

    /**
     * @param $functionName
     * @param $text
     * @param $childItems
     *
     * @return array|null
     */
    private function evaluateFunctionParams($functionName, $text, $childItems)
    {
        if (!preg_match("/^\s*\{\s*$functionName\s*\(/i", (string) $text)) {
            return null;
        }

        $this->log("Matched funcion name: " . $functionName);

        $params = $this->getFunctionParameters($functionName, $text, $childItems);

        $this->log("Resolved parameters for function '$functionName': ");
        $this->logVardump($params);

        return $params;
    }

    /**
     * @param $functionName
     * @param $text
     * @param $childItems
     *
     * @return array
     */
    private function getFunctionParameters($functionName, $text, $childItems)
    {
        $parameters = $this->getParameterArray($functionName, $text);

        $resolvedParameters = array();
        foreach ($parameters as $parameter) {
            $this->log("Resolving parameter '$parameter'");

            $found = false;

            foreach ($childItems as $childItem) {
                if ($parameter == $childItem['value']) {
                    $this->log("Replacing parameter '$parameter' with value '" . $childItem['evaluatedValue'] . "'");
                    $resolvedParameters [] = $childItem['evaluatedValue'];
                    $found = true;
                    break;
                }
            }

            if (!$found) {
                $paramText = $parameter;
                $replaced = false;

                $this->log("Single expression parameter not found, trying to parse multi expression parameter...");
                foreach ($childItems as $childItem) {
                    if (mb_strpos((string) $paramText, (string) $childItem['value']) !== false) {
                        $this->log("Found multi expression part '" . $childItem['value'] . "' in parameter '$paramText'");
                        $this->log("Replacing parameter part '" . $childItem['value'] . "' with value '" . $childItem['evaluatedValue'] . "'");

                        $paramText = str_replace($childItem['value'], $childItem['evaluatedValue'], (string) $paramText);
                        $replaced = true;

                        $this->log("New parameter value '$paramText'");
                    }
                }

                if (!$replaced) {
                    $this->log("Did not found any multi expression part.");
                }

                $resolvedParameters [] = $paramText;
            }
        }

        return $resolvedParameters;
    }

    /**
     * @param $functionName
     * @param $text
     *
     * @return array
     */
    private function getParameterArray($functionName, $text)
    {
        $this->log("Extracting parameters for function '$functionName' ...");

        $parameterText = $this->getParameterText($functionName, $text);

        $characters = preg_split('//u', $parameterText, -1, PREG_SPLIT_NO_EMPTY);
        $terminalLevel = 0;

        $params = array();
        $currentParam = "";
        $charactersCount = is_countable($characters) ? count($characters) : 0;
        for ($i = 0; $i < $charactersCount; $i++) {
            $char = $characters[$i];

            if ($char === FormulaCalculator::START_TERMINAL) {
                $terminalLevel++;
                $currentParam .= $char;
            } else {
                if ($char === FormulaCalculator::END_TERMINAL) {
                    $terminalLevel--;
                    $currentParam .= $char;
                } else {
                    if ($char === FormulaCalculator::PARAMETER_SEPARATOR_TERMINAL) {
                        if ($terminalLevel == 0) {
                            $params [] = $currentParam;
                            $currentParam = "";
                        } else {
                            $currentParam .= $char;
                        }
                    } else {
                        $currentParam .= $char;
                    }
                }
            }
        }

        $params [] = $currentParam;
        $trimmed = array_map('trim', $params);

        $this->log("Extracted parameters:");
        $this->logVardump($params);

        return $trimmed;
    }

    /**
     * @param $functionName
     * @param $text
     *
     * @return string
     */
    private function getParameterText($functionName, $text)
    {
        $parameterText = preg_replace("/^\s*\{\s*" . $functionName . "\s*\(\s*/", "", (string) $text, 1);
        $parameterText = preg_replace("/\s*\)\s*\}\s*$/", "", $parameterText, 1);

        return trim($parameterText);
    }

    /**
     * @param $value
     *
     * @return float
     */
    private function parseFloat($value)
    {
        return (float)str_replace(",", ".", (string) $value);
    }

    /**
     * @param $format
     * @param $datestring
     * @param $ammount
     * @param $type
     * @param bool $isTime
     * @param bool $isAdd
     *
     * @return string
     */
    private function modifyDate($format, $datestring, $ammount, $type, $isTime = false, $isAdd = true)
    {
        $prefix = $isTime ? 'PT' : 'P';

        $datetime = new DateTime($this->getDBFormat($datestring));

        if ($isAdd) {
            $datetime->add(new DateInterval($prefix . $ammount . $type));
        } else {
            $datetime->sub(new DateInterval($prefix . $ammount . $type));
        }

        return $datetime->format($format);
    }

    /**
     * @param $leaf
     *
     * @return int|mixed|string
     */
    private function evaluateLeaf($leaf)
    {
        $evaluated = $leaf;

        if (preg_match("/{P[0-9]+}/i", (string) $leaf)) {
            $parametersCount = is_countable($this->parameters) ? count($this->parameters) : 0;
            for ($i = 0; $i < $parametersCount; $i++) {
                $evaluated = str_replace("{P$i}", $this->parameters[$i], (string) $evaluated);
                $evaluated = str_replace("{p$i}", $this->parameters[$i], $evaluated);
            }
        } else {
            if (preg_match("/{R[0-9]+}/i", (string) $leaf)) {
                $relationParametersCount = is_countable($this->relationParameters) ? count($this->relationParameters) : 0;
                for ($i = 0; $i < $relationParametersCount; $i++) {
                    $evaluated = str_replace("{R$i}", $this->relationParameters[$i], (string) $evaluated);
                    $evaluated = str_replace("{r$i}", $this->relationParameters[$i], $evaluated);
                }
            }
        }

        $evaluated = $this->replaceGlobalVariables($evaluated);

        return $evaluated;
    }

    /**
     * @param $text
     *
     * @return int|string
     */
    private function replaceGlobalVariables($text)
    {
        $evaluated = $text;

        $evaluated = $this->replaceGlobalVariable('GlobalCounter', $evaluated);
        $evaluated = $this->replaceGlobalVariable('GlobalCounterPerUser', $evaluated);
        $evaluated = $this->replaceGlobalVariable('GlobalCounterPerModule', $evaluated);
        $evaluated = $this->replaceGlobalVariable('GlobalCounterPerUserPerModule', $evaluated);
        $evaluated = $this->replaceGlobalVariable('DailyCounter', $evaluated);
        $evaluated = $this->replaceGlobalVariable('DailyCounterPerUser', $evaluated);
        $evaluated = $this->replaceGlobalVariable('DailyCounterPerModule', $evaluated);
        $evaluated = $this->replaceGlobalVariable('DailyCounterPerUserPerModule', $evaluated);

        return $evaluated;
    }

    /**
     * @param $globalVariableType
     * @param $text
     *
     * @return int|string
     */
    private function replaceGlobalVariable($globalVariableType, $text)
    {
        if (preg_match("/^\{$globalVariableType\(/i", (string) $text)) {
            $parameters = $this->getParameterArray($globalVariableType, $text);
            $currentValue = $this->getGlobalVariableConfig($globalVariableType, $parameters[0]);
            $newValue = $currentValue + 1;

            $this->setGlobalVariableConfig($globalVariableType, $parameters[0], $newValue);

            return isset($parameters[1]) ? $this->formatCounter($newValue, $parameters[1]) : $newValue;
        }

        return $text;
    }

    /**
     * @param $globalVariableType
     * @param $parameterText
     *
     * @return int
     */
    private function getGlobalVariableConfig($globalVariableType, $parameterText)
    {
        switch ($globalVariableType) {
            case 'GlobalCounter':
                return $this->configurator->config[FormulaCalculator::CONFIGURATOR_NAME]['GlobalCounter'][$parameterText];
            case 'GlobalCounterPerUser':
                return $this->configurator->config[FormulaCalculator::CONFIGURATOR_NAME]['GlobalCounterPerUser'][$this->creatorUserId][$parameterText];
            case 'GlobalCounterPerModule':
                return $this->configurator->config[FormulaCalculator::CONFIGURATOR_NAME]['GlobalCounterPerModule'][$this->currentModule][$parameterText];
            case 'GlobalCounterPerUserPerModule':
                return $this->configurator->config[FormulaCalculator::CONFIGURATOR_NAME]['GlobalCounterPerUserPerModule'][$this->creatorUserId][$this->currentModule][$parameterText];

            case 'DailyCounter':
                if ($this->configurator->config[FormulaCalculator::CONFIGURATOR_NAME]['DailyCounter'][$parameterText]['date'] ===
                    date('Y-m-d')
                ) {
                    return $this->configurator->config[FormulaCalculator::CONFIGURATOR_NAME]['DailyCounter'][$parameterText]['value'];
                }

                return 0;

            case 'DailyCounterPerUser':
                if ($this->configurator->config[FormulaCalculator::CONFIGURATOR_NAME]['DailyCounterPerUser'][$this->creatorUserId][$parameterText]['date'] ===
                    date('Y-m-d')
                ) {
                    return $this->configurator->config[FormulaCalculator::CONFIGURATOR_NAME]['DailyCounterPerUser'][$this->creatorUserId][$parameterText]['value'];
                }

                return 0;

            case 'DailyCounterPerModule':
                if ($this->configurator->config[FormulaCalculator::CONFIGURATOR_NAME]['DailyCounterPerUser'][$this->currentModule][$parameterText]['date'] ===
                    date('Y-m-d')
                ) {
                    return $this->configurator->config[FormulaCalculator::CONFIGURATOR_NAME]['DailyCounterPerUser'][$this->currentModule][$parameterText]['value'];
                }

                return 0;

            case 'DailyCounterPerUserPerModule':
                if ($this->configurator->config[FormulaCalculator::CONFIGURATOR_NAME]['DailyCounterPerUserPerModule'][$this->creatorUserId][$this->currentModule][$parameterText]['date'] ===
                    date('Y-m-d')
                ) {
                    return $this->configurator->config[FormulaCalculator::CONFIGURATOR_NAME]['DailyCounterPerUserPerModule'][$this->creatorUserId][$this->currentModule][$parameterText]['value'];
                }

                return 0;

        }
    }

    /**
     * @param $globalVariableType
     * @param $parameterText
     * @param $value
     */
    private function setGlobalVariableConfig($globalVariableType, $parameterText, $value)
    {
        switch ($globalVariableType) {
            case 'GlobalCounter':
                $this->configurator->config[FormulaCalculator::CONFIGURATOR_NAME]['GlobalCounter'][$parameterText] = $value;
                break;
            case 'GlobalCounterPerUser':
                $this->configurator->config[FormulaCalculator::CONFIGURATOR_NAME]['GlobalCounterPerUser'][$this->creatorUserId][$parameterText] = $value;
                break;
            case 'GlobalCounterPerModule':
                $this->configurator->config[FormulaCalculator::CONFIGURATOR_NAME]['GlobalCounterPerModule'][$this->currentModule][$parameterText] = $value;
                break;
            case 'GlobalCounterPerUserPerModule':
                $this->configurator->config[FormulaCalculator::CONFIGURATOR_NAME]['GlobalCounterPerUserPerModule'][$this->creatorUserId][$this->currentModule][$parameterText] = $value;
                break;
            case 'DailyCounter':
                $this->configurator->config[FormulaCalculator::CONFIGURATOR_NAME]['DailyCounter'][$parameterText]['date'] = date('Y-m-d');
                $this->configurator->config[FormulaCalculator::CONFIGURATOR_NAME]['DailyCounter'][$parameterText]['value'] = $value;
                break;
            case 'DailyCounterPerUser':
                $this->configurator->config[FormulaCalculator::CONFIGURATOR_NAME]['DailyCounterPerUser'][$this->creatorUserId][$parameterText]['date'] = date('Y-m-d');
                $this->configurator->config[FormulaCalculator::CONFIGURATOR_NAME]['DailyCounterPerUser'][$this->creatorUserId][$parameterText]['value'] = $value;
                break;
            case 'DailyCounterPerModule':
                $this->configurator->config[FormulaCalculator::CONFIGURATOR_NAME]['DailyCounterPerUser'][$this->currentModule][$parameterText]['date'] = date('Y-m-d');
                $this->configurator->config[FormulaCalculator::CONFIGURATOR_NAME]['DailyCounterPerUser'][$this->currentModule][$parameterText]['value'] = $value;
                break;
            case 'DailyCounterPerUserPerModule':
                $this->configurator->config[FormulaCalculator::CONFIGURATOR_NAME]['DailyCounterPerUserPerModule'][$this->creatorUserId][$this->currentModule][$parameterText]['date'] = date('Y-m-d');
                $this->configurator->config[FormulaCalculator::CONFIGURATOR_NAME]['DailyCounterPerUserPerModule'][$this->creatorUserId][$this->currentModule][$parameterText]['value'] = $value;
                break;
        }

        $this->configurator->saveConfig();
    }

    /**
     * @param $value
     * @param $digits
     *
     * @return string
     */
    private function formatCounter($value, $digits)
    {
        return sprintf("%0" . $digits . "d", $value);
    }

    /**
     * Outputs date and datetime values in DB format
     *
     * @param String $date
     * @return String
     */
    private function getDBFormat($date) {
        // 1) If WF is thrown by the after_save LH, the bean is already loaded and the date/datetime value
        // is properly formatted, so will only change the timezone value from UTC to user's one.
        // 2) If WF is run by the scheduler task, will change date/datetime value to DB format.
        $formatDate = 'Y-m-d';
        $validDate = DateTime::createFromFormat($formatDate, $date);
        $formatDateTime = 'Y-m-d H:i:s';
        $validDateTime = DateTime::createFromFormat($formatDateTime, $date);
        if ($validDate && $validDate->format($formatDate) === $date) {
            // Nothing to do
            return $date;
        } else if ($validDateTime && $validDateTime->format($formatDateTime) === $date) {
            // Set TZ to user's TZ
            global $timedate, $current_user;
            $date = $timedate->fromDb($date);
            $date = $timedate->tzUser($date, $current_user);
            return $date->format('Y-m-d H:i:s');
        } else { // In this case the WF is run by the cron
            global $current_user, $timedate;
            if(strpos($date, " ") !== false){
                $type = 'datetime';
            } else{
                $type = 'date';
            }
            $date = $timedate->fromUserType($date, $type, $current_user);
            if ($date) {
                return $date->asDb(false);
            }
            return null;
        }
    }

}
