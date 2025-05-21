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

use SuiteCRM\LangText;

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

require_once __DIR__ . '/Email.php';
require_once __DIR__ . '/EmailValidatorException.php';

/**
 * EmailFromValidator
 * Specially for validate and handling any confusion From Address / From Name issue.
 *
 * @author gyula
 */
#[\AllowDynamicProperties]
class EmailFromValidator
{
    public const ERR_FIELD_FROM_IS_NOT_SET = 1;
    public const ERR_FIELD_FROM_IS_EMPTY = 2;
    public const ERR_FIELD_FROM_IS_INVALID = 3;
    public const ERR_FIELD_FROM_ADDR_IS_NOT_SET = 4;
    public const ERR_FIELD_FROM_ADDR_IS_EMPTY = 5;
    public const ERR_FIELD_FROM_ADDR_IS_INVALID = 6;
    public const ERR_FIELD_FROMNAME_IS_NOT_SET = 7;
    public const ERR_FIELD_FROMNAME_IS_EMPTY = 8;
    public const ERR_FIELD_FROMNAME_IS_INVALID = 9;
    public const ERR_FIELD_FROM_NAME_IS_NOT_SET = 10;
    public const ERR_FIELD_FROM_NAME_IS_EMPTY = 11;
    public const ERR_FIELD_FROM_NAME_IS_INVALID = 12;
    public const ERR_FIELD_FROM_ADDR_NAME_IS_NOT_SET = 13;
    public const ERR_FIELD_FROM_ADDR_NAME_IS_EMPTY = 14;
    public const ERR_FIELD_FROM_ADDR_NAME_IS_INVALID = 15;
    public const ERR_FIELD_FROM_ADDR_NAME_DOESNT_MATCH_REGEX = 16;
    public const ERR_FIELD_FROM_ADDR_NAME_INVALID_NAME_PART = 17;
    public const ERR_FIELD_FROM_ADDR_NAME_INVALID_EMAIL_PART = 18;
    public const ERR_FIELD_FROM_ADDR_NAME_INVALID_EMAIL_PART_TO_FIELD_FROM = 19;
    public const ERR_FIELD_FROM_ADDR_NAME_INVALID_EMAIL_PART_TO_FIELD_FROM_ADDR = 20;
    public const ERR_FIELD_FROM_ADDR_NAME_INVALID_EMAIL_PART_TO_FIELD_FROMNAME = 21;
    public const ERR_FIELD_FROM_ADDR_NAME_INVALID_EMAIL_PART_TO_FIELD_FROM_NAME = 22;

    public const EX_ERROR_CODE_TYRE_IS_INCORRECT = 100;
    public const EX_ERROR_CODE_IS_NOT_IMPLEMENTED = 101;

    /**
     *
     * @var Email;
     */
    protected $email;

    /**
     *
     * @var array
     */
    protected $errors;


    /**
     * Specially use before email sending.
     *
     * @param Email $email
     * @param bool $tryToFix
     * @return bool
     * @throws EmailValidatorException
     */
    public function isValid(Email $email, $tryToFix = true)
    {
        $this->setEmail($email);
        $this->clearErrors();
        $this->addErrors($this->validateFrom());
        $this->addErrors($this->validateFromAddr());
        $this->addErrors($this->validateFromName());
        $this->addErrors($this->validateFrom_Name());
        $this->addErrors($this->validateFromAddrName());
        // Should be finished the rest fields like to, to address, cc, bcc etc.. (reply-to??)
//        $this->addErrors($this->validateTo());
//        $this->addErrors($this->validateCCs());
//        $this->addErrors($this->validateBCCs());
        $valid = !$this->hasErrors();
        if (!$valid && $tryToFix) {
            $valid = !$this->hasErrors();
        }

        return $valid;
    }

    /**
     *
     * @param Email $email
     */
    protected function setEmail(Email $email)
    {
        $this->email = $email;
    }

    /**
     *
     * @return Email
     * @throws EmailValidatorException
     */
    protected function getEmail()
    {
        if (!$this->email) {
            throw new EmailValidatorException(
                'Trying to get Email but previously is not set.',
                EmailValidatorException::EMAIL_IS_NOT_SET
            );
        }
        if (!($this->email instanceof Email)) {
            throw new EmailValidatorException(
                'Trying to get Email but object type is incorrect:' . gettype($this->email),
                EmailValidatorException::EMAIL_ISNT_EMAILOBJ
            );
        }

        return $this->email;
    }

    /**
     *
     */
    protected function clearErrors()
    {
        $this->errors = [];
    }

    /**
     *
     * @param $error
     * @throws InvalidArgumentException
     */
    protected function addError($error)
    {
        if ($error !== (int)$error) {
            throw new InvalidArgumentException(
                'Error code should be an integer, ' . gettype($error) . ' given',
                self::EX_ERROR_CODE_TYRE_IS_INCORRECT
            );
        }
        if (!in_array($error, $this->errors)) {
            $this->errors[] = $error;
        }
    }

    /**
     *
     * @param array $errors
     */
    protected function addErrors($errors)
    {
        $this->errors = array_merge($this->errors, $errors);
        $this->errors = array_unique($this->errors);
    }

    /**
     *
     * @return array
     */
    public function getErrors()
    {
        $errorsArray = $this->errors;
        $this->clearErrors();

        return $errorsArray;
    }

    /**
     * @return array
     * @throws \SuiteCRM\ErrorMessageException
     */
    public function getErrorsAsText()
    {
        $txts = [];
        $errorsArray = $this->getErrors();
        foreach ($errorsArray as $error) {
            $txts[] = $this->getErrorAsText($error);
        }

        return [
            'messages' => implode("\n", $txts),
            'codes' => implode(', ', $errorsArray)
        ];
    }

    /**
     * @param $error
     * @return string
     * @throws \SuiteCRM\ErrorMessageException
     */
    protected function getErrorAsText($error)
    {
        if ($error !== (int)$error) {
            throw new InvalidArgumentException(
                'Error code should be an integer, ' . gettype($error) . ' given',
                self::EX_ERROR_CODE_TYRE_IS_INCORRECT
            );
        }
        $lbl = $this->getErrorTextLabel($error);
        return LangText::get($lbl, null, LangText::USING_ALL_STRINGS, true, false, 'Emails');
    }

    /**
     *
     * @param int $error
     * @return string
     * @throws InvalidArgumentException
     */
    protected function getErrorTextLabel($error)
    {
        if ($error !== (int)$error) {
            throw new InvalidArgumentException(
                'Error code should be an integer, ' . gettype($error) . ' given',
                self::EX_ERROR_CODE_TYRE_IS_INCORRECT
            );
        }
        switch ($error) {
            case self::ERR_FIELD_FROM_IS_NOT_SET:
                $lbl = 'ERR_FIELD_FROM_IS_NOT_SET';
                break;
            case self::ERR_FIELD_FROM_IS_EMPTY:
                $lbl = 'ERR_FIELD_FROM_IS_EMPTY';
                break;
            case self::ERR_FIELD_FROM_IS_INVALID:
                $lbl = 'ERR_FIELD_FROM_IS_INVALID';
                break;
            case self::ERR_FIELD_FROM_ADDR_IS_NOT_SET:
                $lbl = 'ERR_FIELD_FROM_ADDR_IS_NOT_SET';
                break;
            case self::ERR_FIELD_FROM_ADDR_IS_EMPTY:
                $lbl = 'ERR_FIELD_FROM_ADDR_IS_EMPTY';
                break;
            case self::ERR_FIELD_FROM_ADDR_IS_INVALID:
                $lbl = 'ERR_FIELD_FROM_ADDR_IS_INVALID';
                break;
            case self::ERR_FIELD_FROMNAME_IS_NOT_SET:
                $lbl = 'ERR_FIELD_FROMNAME_IS_NOT_SET';
                break;
            case self::ERR_FIELD_FROMNAME_IS_EMPTY:
                $lbl = 'ERR_FIELD_FROMNAME_IS_EMPTY';
                break;
            case self::ERR_FIELD_FROMNAME_IS_INVALID:
                $lbl = 'ERR_FIELD_FROMNAME_IS_INVALID';
                break;
            case self::ERR_FIELD_FROM_NAME_IS_NOT_SET:
                $lbl = 'ERR_FIELD_FROM_NAME_IS_NOT_SET';
                break;
            case self::ERR_FIELD_FROM_NAME_IS_EMPTY:
                $lbl = 'ERR_FIELD_FROM_NAME_IS_EMPTY';
                break;
            case self::ERR_FIELD_FROM_NAME_IS_INVALID:
                $lbl = 'ERR_FIELD_FROM_NAME_IS_INVALID';
                break;
            case self::ERR_FIELD_FROM_ADDR_NAME_IS_NOT_SET:
                $lbl = 'ERR_FIELD_FROM_ADDR_NAME_IS_NOT_SET';
                break;
            case self::ERR_FIELD_FROM_ADDR_NAME_IS_EMPTY:
                $lbl = 'ERR_FIELD_FROM_ADDR_NAME_IS_EMPTY';
                break;
            case self::ERR_FIELD_FROM_ADDR_NAME_IS_INVALID:
                $lbl = 'ERR_FIELD_FROM_ADDR_NAME_IS_INVALID';
                break;
            case self::ERR_FIELD_FROM_ADDR_NAME_DOESNT_MATCH_REGEX:
                $lbl = 'ERR_FIELD_FROM_ADDR_NAME_DOESNT_MATCH_REGEX';
                break;
            case self::ERR_FIELD_FROM_ADDR_NAME_INVALID_NAME_PART:
                $lbl = 'ERR_FIELD_FROM_ADDR_NAME_INVALID_NAME_PART';
                break;
            case self::ERR_FIELD_FROM_ADDR_NAME_INVALID_EMAIL_PART:
                $lbl = 'ERR_FIELD_FROM_ADDR_NAME_INVALID_EMAIL_PART';
                break;
            case self::ERR_FIELD_FROM_ADDR_NAME_INVALID_EMAIL_PART_TO_FIELD_FROM:
                $lbl = 'ERR_FIELD_FROM_ADDR_NAME_INVALID_EMAIL_PART_TO_FIELD_FROM';
                break;
            case self::ERR_FIELD_FROM_ADDR_NAME_INVALID_EMAIL_PART_TO_FIELD_FROM_ADDR:
                $lbl = 'ERR_FIELD_FROM_ADDR_NAME_INVALID_EMAIL_PART_TO_FIELD_FROM_ADDR';
                break;
            case self::ERR_FIELD_FROM_ADDR_NAME_INVALID_EMAIL_PART_TO_FIELD_FROMNAME:
                $lbl = 'ERR_FIELD_FROM_ADDR_NAME_INVALID_EMAIL_PART_TO_FIELD_FROMNAME';
                break;
            case self::ERR_FIELD_FROM_ADDR_NAME_INVALID_EMAIL_PART_TO_FIELD_FROM_NAME:
                $lbl = 'ERR_FIELD_FROM_ADDR_NAME_INVALID_EMAIL_PART_TO_FIELD_FROM_NAME';
                break;
            default:
                throw new InvalidArgumentException(
                    'Error code is not implemented: ' . $error,
                    self::EX_ERROR_CODE_IS_NOT_IMPLEMENTED
                );
        }

        return $lbl;
    }

    /**
     *
     * @return bool
     */
    protected function hasErrors()
    {
        return !empty($this->errors);
    }

    /**
     *
     * @param string $emailAddress
     * @return bool
     */
    protected function isValidEmailAddress($emailAddress)
    {
        return is_string($emailAddress) && isValidEmailAddress($emailAddress, '', false, '');
    }

    /**
     *
     * @param string $nonEmailAddress
     * @return boolean
     */
    protected function isValidNonEmailAddress($nonEmailAddress)
    {
        $valid = true;
        if (!is_string($nonEmailAddress) || !$nonEmailAddress || isValidEmailAddress($nonEmailAddress, '', false, '')) {
            $valid = false;
        }

        return $valid;
    }


    /**
     *
     * @param string $fromAddress
     * @return bool
     */
    protected function isValidFromAddress($fromAddress)
    {
        return $this->isValidEmailAddress($fromAddress);
    }

    /**
     *
     * @param string $fromName
     * @return boolean
     */
    protected function isValidFromName($fromName)
    {
        return $this->isValidNonEmailAddress($fromName);
    }

    /**
     *
     * @param string $fromAddrName
     * @return boolean
     * @throws EmailValidatorException
     */
    protected function isValidFromAddrName($fromAddrName)
    {
        $valid = false;
        $matches = null;
        $results = preg_match('/([^<]+)\s+<([^>]+)>/', $fromAddrName, $matches);
        if ($results === false) {
            throw new EmailValidatorException(
                'preg_match error occurred at from_addr_name check.',
                EmailValidatorException::PREG_MATCH_ERROR_AT_FROMADDRNAME
            );
        }
        if (!$results) {
            $this->addError(self::ERR_FIELD_FROM_ADDR_NAME_DOESNT_MATCH_REGEX);
        } else {
            $name = $matches[1];
            $emailAddress = $matches[2];

            $ok = true;
            if (!$this->isValidNonEmailAddress($name)) {
                $this->addError(self::ERR_FIELD_FROM_ADDR_NAME_INVALID_NAME_PART);
                $ok = false;
            }

            if (!$this->isValidEmailAddress($emailAddress)) {
                $this->addError(self::ERR_FIELD_FROM_ADDR_NAME_INVALID_EMAIL_PART);
                $ok = false;
            }

            $emailObj = $this->getEmail();

            if (isset($emailObj->From) && $emailAddress !== $emailObj->From) {
                $this->addError(self::ERR_FIELD_FROM_ADDR_NAME_INVALID_EMAIL_PART_TO_FIELD_FROM);
                $ok = false;
            }

            if (isset($emailObj->from_addr) && $emailAddress !== $emailObj->from_addr) {
                $this->addError(self::ERR_FIELD_FROM_ADDR_NAME_INVALID_EMAIL_PART_TO_FIELD_FROM_ADDR);
                $ok = false;
            }

            if (isset($emailObj->FromName) && $name !== $emailObj->FromName) {
                $this->addError(self::ERR_FIELD_FROM_ADDR_NAME_INVALID_EMAIL_PART_TO_FIELD_FROMNAME);
                $ok = false;
            }

            if (isset($emailObj->from_name) && $name !== $emailObj->from_name) {
                $this->addError(self::ERR_FIELD_FROM_ADDR_NAME_INVALID_EMAIL_PART_TO_FIELD_FROM_NAME);
                $ok = false;
            }

            if ($ok) {
                $valid = true;
            }
        }

        return $valid;
    }


    /**
     * validate field 'From' - should be a valid email address
     *
     * @return array
     * @throws EmailValidatorException
     */
    protected function validateFrom()
    {
        $emailAddress = $this->getEmail();
        if (!isset($emailAddress->From)) {
            $this->addError(self::ERR_FIELD_FROM_IS_NOT_SET);
        } elseif (!$emailAddress->From) {
            $this->addError(self::ERR_FIELD_FROM_IS_EMPTY);
        } elseif (!$this->isValidFromAddress($emailAddress->From)) {
            $this->addError(self::ERR_FIELD_FROM_IS_INVALID);
        }


        return $this->getErrors();
    }

    /**
     * validate field 'from_addr' - should be a valid email address
     *
     * @return array
     * @throws EmailValidatorException
     */
    protected function validateFromAddr()
    {
        $emailAddress = $this->getEmail();
        if (!isset($emailAddress->from_addr)) {
            $this->addError(self::ERR_FIELD_FROM_ADDR_IS_NOT_SET);
        } elseif (!$emailAddress->from_addr) {
            $this->addError(self::ERR_FIELD_FROM_ADDR_IS_EMPTY);
        } elseif (!$this->isValidFromAddress($emailAddress->from_addr)) {
            $this->addError(self::ERR_FIELD_FROM_ADDR_IS_INVALID);
        }

        return $this->getErrors();
    }

    /**
     * validate field 'FromName' - should be a valid name string
     *
     * @return array
     * @throws EmailValidatorException
     */
    protected function validateFromName()
    {
        $emailAddress = $this->getEmail();
        if (!isset($emailAddress->FromName)) {
            $this->addError(self::ERR_FIELD_FROMNAME_IS_NOT_SET);
        } elseif (!$emailAddress->FromName) {
            $this->addError(self::ERR_FIELD_FROMNAME_IS_EMPTY);
        } elseif (!$this->isValidFromName($emailAddress->FromName)) {
            $this->addError(self::ERR_FIELD_FROMNAME_IS_INVALID);
        }

        return $this->getErrors();
    }

    /**
     * validate field 'from_name' - should be a valid name string
     *
     * @return array
     * @throws EmailValidatorException
     */
    protected function validateFrom_Name()
    {
        $emailAddress = $this->getEmail();
        if (!isset($emailAddress->from_name)) {
            $this->addError(self::ERR_FIELD_FROM_NAME_IS_NOT_SET);
        } elseif (!$emailAddress->from_name) {
            $this->addError(self::ERR_FIELD_FROM_NAME_IS_EMPTY);
        } elseif (!$this->isValidFromName($emailAddress->from_name)) {
            $this->addError(self::ERR_FIELD_FROM_NAME_IS_INVALID);
        }

        return $this->getErrors();
    }

    /**
     * validate field 'from_addr_name' - should be a valid name string and email address pair
     * where email address in between '<' and '>' characters
     *
     * @return array
     * @throws EmailValidatorException
     */
    protected function validateFromAddrName()
    {
        $emailAddress = $this->getEmail();
        if (!isset($emailAddress->from_addr_name)) {
            $this->addError(self::ERR_FIELD_FROM_ADDR_NAME_IS_NOT_SET);
        } elseif (!$emailAddress->from_addr_name) {
            $this->addError(self::ERR_FIELD_FROM_ADDR_NAME_IS_EMPTY);
        } elseif (!$this->isValidFromAddrName($emailAddress->from_addr_name)) {
            $this->addError(self::ERR_FIELD_FROM_ADDR_NAME_IS_INVALID);
        }

        return $this->getErrors();
    }
}
