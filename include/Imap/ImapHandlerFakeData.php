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

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

/**
 * ImapHandlerFakeData
 *
 * For tests only, it deals fake return values for fake calls on an IMAP wrapper.
 *
 * @author gyula
 * @todo using common Call faker as base class
 */
#[\AllowDynamicProperties]
class ImapHandlerFakeData
{
    public const ERR_NO_MATCH_ARGS = 1;
    public const ERR_CALL_NOT_FOUND = 2;
    public const ERR_CALL_ALREDY_EXISTS = 3;
    public const ERR_CALL_NOT_EXISTS = 4;
    public const ERR_CALL_REMOVE = 5;
    public const ERR_WRONG_TESTSET = 6;
    
    /**
     *
     * @var array
     */
    protected $calls = [];
    
    /**
     *
     * @param array|null $args
     * @return string
     */
    protected function encodeArgs($args = null)
    {
        return $encoded = md5(serialize($args));
    }
    
    /**
     *
     * @param string $name
     * @param string $argsEncoded
     * @return mixed
     */
    protected function getNextCallReturn($name, $argsEncoded)
    {
        if (!is_array($this->calls[$name][$argsEncoded])) {
            throw new Exception('Fake handler given an incorrect data. Returns should be an array at name: ' . $name, self::ERR_WRONG_TESTSET);
        }
        $ret = array_shift($this->calls[$name][$argsEncoded]);
        $this->calls[$name][$argsEncoded] = array_values($this->calls[$name][$argsEncoded]);
        if (empty($this->calls[$name][$argsEncoded])) {
            // using the last element forever..
            $this->calls[$name][$argsEncoded] = [$ret];
        }
        return $ret;
    }
    
    /**
     *
     * @param string $name
     * @param array|null $args
     * @return mixed
     * @throws Exception
     */
    protected function getCall($name, $args = null)
    {
        if (array_key_exists($name, $this->calls)) {
            $argsEncoded = $this->encodeArgs($args);
            if (array_key_exists($argsEncoded, $this->calls[$name])) {
                $ret = $this->getNextCallReturn($name, $argsEncoded);
                return $ret;
            } else {
                throw new Exception('Fake caller has not matched arguments for this call: ' . $name . "\nArguments was: " . print_r($args, true), self::ERR_NO_MATCH_ARGS);
            }
        } else {
            throw new Exception('Fake call does not exists for this function call: ' . $name . "\nwith specific arguments:\n" . print_r($args, true), self::ERR_CALL_NOT_FOUND);
        }
    }
    
    /**
     *
     * @param string $name
     * @param array|null $args
     * @return mixed
     * @throws Exception
     */
    public function call($name, $args = null)
    {
        $ret = $this->getCall($name, $args);
        if (is_object($ret) && ($ret instanceof Closure)) {
            $out = $ret();
        } else {
            $out = $ret;
        }
        return $out;
    }
    
    /**
     *
     * @param string $name
     * @param array|null $args
     * @param mixed|null $ret
     * @throws Exception
     */
    public function add($name, $args = null, $ret = null)
    {
        $argsEncoded = $this->encodeArgs($args);
        if (isset($this->calls[$name][$argsEncoded])) {
            LoggerManager::getLogger()->warn('Fake call already exists with given arguments: ' . $name . ', hint: remove first, use ' . self::class . '::remove(...)');
            $this->remove($name, $args);
        }
        $this->calls[$name][$argsEncoded] = $ret;
    }
    
    /**
     *
     * @param string $name
     * @param array|null $args
     * @throws Exception
     */
    public function remove($name, $args = null)
    {
        $argsEncoded = $this->encodeArgs($args);
        if (!isset($this->calls[$name][$argsEncoded])) {
            throw new Exception('Trying to remove a fake call but it is not exists: ' . $name, self::ERR_CALL_NOT_EXISTS);
        }
        unset($this->calls[$name][$argsEncoded]);
    }
    
    /**
     *
     */
    public function reset()
    {
        $this->calls = null;
    }
    
    /**
     *
     * @param string $name
     * @param array|null $args
     * @throws Exception
     */
    public function set($name, $args = null)
    {
        try {
            $this->remove($name, $args);
        } catch (Exception $e) {
            if ($e->getCode() != self::ERR_CALL_NOT_EXISTS) {
                throw new Exception('Call remove error', self::ERR_CALL_REMOVE, $e);
            }
        }
        $this->add($name, $args);
    }
    
    /**
     * Following example when ImapHandlerFake::open() called and imitate a success IMAP connection
     *
     * @param array $calls
     */
    public function retrieve($calls)
    {
        foreach ($calls as $name => $call) {
            if (empty($call)) {
                $call = [[]];
            }
            foreach ($call as $param) {
                $args = isset($param['args']) ? $param['args'] : null;
                $ret = isset($param['return']) ? $param['return'] : [null];
                $this->add($name, $args, $ret);
            }
        }
    }
}
