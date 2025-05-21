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

namespace Google\Service\SQLAdmin;

class ImportContextBakImportOptions extends \Google\Model
{
  /**
   * @var string
   */
  public $bakType;
  protected $encryptionOptionsType = ImportContextBakImportOptionsEncryptionOptions::class;
  protected $encryptionOptionsDataType = '';
  /**
   * @var bool
   */
  public $noRecovery;
  /**
   * @var bool
   */
  public $recoveryOnly;
  /**
   * @var string
   */
  public $stopAt;
  /**
   * @var string
   */
  public $stopAtMark;
  /**
   * @var bool
   */
  public $striped;

  /**
   * @param string
   */
  public function setBakType($bakType)
  {
    $this->bakType = $bakType;
  }
  /**
   * @return string
   */
  public function getBakType()
  {
    return $this->bakType;
  }
  /**
   * @param ImportContextBakImportOptionsEncryptionOptions
   */
  public function setEncryptionOptions(ImportContextBakImportOptionsEncryptionOptions $encryptionOptions)
  {
    $this->encryptionOptions = $encryptionOptions;
  }
  /**
   * @return ImportContextBakImportOptionsEncryptionOptions
   */
  public function getEncryptionOptions()
  {
    return $this->encryptionOptions;
  }
  /**
   * @param bool
   */
  public function setNoRecovery($noRecovery)
  {
    $this->noRecovery = $noRecovery;
  }
  /**
   * @return bool
   */
  public function getNoRecovery()
  {
    return $this->noRecovery;
  }
  /**
   * @param bool
   */
  public function setRecoveryOnly($recoveryOnly)
  {
    $this->recoveryOnly = $recoveryOnly;
  }
  /**
   * @return bool
   */
  public function getRecoveryOnly()
  {
    return $this->recoveryOnly;
  }
  /**
   * @param string
   */
  public function setStopAt($stopAt)
  {
    $this->stopAt = $stopAt;
  }
  /**
   * @return string
   */
  public function getStopAt()
  {
    return $this->stopAt;
  }
  /**
   * @param string
   */
  public function setStopAtMark($stopAtMark)
  {
    $this->stopAtMark = $stopAtMark;
  }
  /**
   * @return string
   */
  public function getStopAtMark()
  {
    return $this->stopAtMark;
  }
  /**
   * @param bool
   */
  public function setStriped($striped)
  {
    $this->striped = $striped;
  }
  /**
   * @return bool
   */
  public function getStriped()
  {
    return $this->striped;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(ImportContextBakImportOptions::class, 'Google_Service_SQLAdmin_ImportContextBakImportOptions');
