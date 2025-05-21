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

namespace Google\Service\Gmail;

class CseKeyPair extends \Google\Collection
{
  protected $collection_key = 'subjectEmailAddresses';
  /**
   * @var string
   */
  public $disableTime;
  /**
   * @var string
   */
  public $enablementState;
  /**
   * @var string
   */
  public $keyPairId;
  /**
   * @var string
   */
  public $pem;
  /**
   * @var string
   */
  public $pkcs7;
  protected $privateKeyMetadataType = CsePrivateKeyMetadata::class;
  protected $privateKeyMetadataDataType = 'array';
  /**
   * @var string[]
   */
  public $subjectEmailAddresses;

  /**
   * @param string
   */
  public function setDisableTime($disableTime)
  {
    $this->disableTime = $disableTime;
  }
  /**
   * @return string
   */
  public function getDisableTime()
  {
    return $this->disableTime;
  }
  /**
   * @param string
   */
  public function setEnablementState($enablementState)
  {
    $this->enablementState = $enablementState;
  }
  /**
   * @return string
   */
  public function getEnablementState()
  {
    return $this->enablementState;
  }
  /**
   * @param string
   */
  public function setKeyPairId($keyPairId)
  {
    $this->keyPairId = $keyPairId;
  }
  /**
   * @return string
   */
  public function getKeyPairId()
  {
    return $this->keyPairId;
  }
  /**
   * @param string
   */
  public function setPem($pem)
  {
    $this->pem = $pem;
  }
  /**
   * @return string
   */
  public function getPem()
  {
    return $this->pem;
  }
  /**
   * @param string
   */
  public function setPkcs7($pkcs7)
  {
    $this->pkcs7 = $pkcs7;
  }
  /**
   * @return string
   */
  public function getPkcs7()
  {
    return $this->pkcs7;
  }
  /**
   * @param CsePrivateKeyMetadata[]
   */
  public function setPrivateKeyMetadata($privateKeyMetadata)
  {
    $this->privateKeyMetadata = $privateKeyMetadata;
  }
  /**
   * @return CsePrivateKeyMetadata[]
   */
  public function getPrivateKeyMetadata()
  {
    return $this->privateKeyMetadata;
  }
  /**
   * @param string[]
   */
  public function setSubjectEmailAddresses($subjectEmailAddresses)
  {
    $this->subjectEmailAddresses = $subjectEmailAddresses;
  }
  /**
   * @return string[]
   */
  public function getSubjectEmailAddresses()
  {
    return $this->subjectEmailAddresses;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(CseKeyPair::class, 'Google_Service_Gmail_CseKeyPair');
