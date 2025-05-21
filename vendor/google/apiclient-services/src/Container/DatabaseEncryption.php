<?php
/*
 * Copyright 2014 Google Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy of
 * the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations under
 * the License.
 */

namespace Google\Service\Container;

class DatabaseEncryption extends \Google\Collection
{
  protected $collection_key = 'lastOperationErrors';
  /**
   * @var string
   */
  public $currentState;
  /**
   * @var string[]
   */
  public $decryptionKeys;
  /**
   * @var string
   */
  public $keyName;
  protected $lastOperationErrorsType = OperationError::class;
  protected $lastOperationErrorsDataType = 'array';
  /**
   * @var string
   */
  public $state;

  /**
   * @param string
   */
  public function setCurrentState($currentState)
  {
    $this->currentState = $currentState;
  }
  /**
   * @return string
   */
  public function getCurrentState()
  {
    return $this->currentState;
  }
  /**
   * @param string[]
   */
  public function setDecryptionKeys($decryptionKeys)
  {
    $this->decryptionKeys = $decryptionKeys;
  }
  /**
   * @return string[]
   */
  public function getDecryptionKeys()
  {
    return $this->decryptionKeys;
  }
  /**
   * @param string
   */
  public function setKeyName($keyName)
  {
    $this->keyName = $keyName;
  }
  /**
   * @return string
   */
  public function getKeyName()
  {
    return $this->keyName;
  }
  /**
   * @param OperationError[]
   */
  public function setLastOperationErrors($lastOperationErrors)
  {
    $this->lastOperationErrors = $lastOperationErrors;
  }
  /**
   * @return OperationError[]
   */
  public function getLastOperationErrors()
  {
    return $this->lastOperationErrors;
  }
  /**
   * @param string
   */
  public function setState($state)
  {
    $this->state = $state;
  }
  /**
   * @return string
   */
  public function getState()
  {
    return $this->state;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(DatabaseEncryption::class, 'Google_Service_Container_DatabaseEncryption');
