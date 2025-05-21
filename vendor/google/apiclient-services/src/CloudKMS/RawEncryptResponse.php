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

namespace Google\Service\CloudKMS;

class RawEncryptResponse extends \Google\Model
{
  /**
   * @var string
   */
  public $ciphertext;
  /**
   * @var string
   */
  public $ciphertextCrc32c;
  /**
   * @var string
   */
  public $initializationVector;
  /**
   * @var string
   */
  public $initializationVectorCrc32c;
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $protectionLevel;
  /**
   * @var int
   */
  public $tagLength;
  /**
   * @var bool
   */
  public $verifiedAdditionalAuthenticatedDataCrc32c;
  /**
   * @var bool
   */
  public $verifiedInitializationVectorCrc32c;
  /**
   * @var bool
   */
  public $verifiedPlaintextCrc32c;

  /**
   * @param string
   */
  public function setCiphertext($ciphertext)
  {
    $this->ciphertext = $ciphertext;
  }
  /**
   * @return string
   */
  public function getCiphertext()
  {
    return $this->ciphertext;
  }
  /**
   * @param string
   */
  public function setCiphertextCrc32c($ciphertextCrc32c)
  {
    $this->ciphertextCrc32c = $ciphertextCrc32c;
  }
  /**
   * @return string
   */
  public function getCiphertextCrc32c()
  {
    return $this->ciphertextCrc32c;
  }
  /**
   * @param string
   */
  public function setInitializationVector($initializationVector)
  {
    $this->initializationVector = $initializationVector;
  }
  /**
   * @return string
   */
  public function getInitializationVector()
  {
    return $this->initializationVector;
  }
  /**
   * @param string
   */
  public function setInitializationVectorCrc32c($initializationVectorCrc32c)
  {
    $this->initializationVectorCrc32c = $initializationVectorCrc32c;
  }
  /**
   * @return string
   */
  public function getInitializationVectorCrc32c()
  {
    return $this->initializationVectorCrc32c;
  }
  /**
   * @param string
   */
  public function setName($name)
  {
    $this->name = $name;
  }
  /**
   * @return string
   */
  public function getName()
  {
    return $this->name;
  }
  /**
   * @param string
   */
  public function setProtectionLevel($protectionLevel)
  {
    $this->protectionLevel = $protectionLevel;
  }
  /**
   * @return string
   */
  public function getProtectionLevel()
  {
    return $this->protectionLevel;
  }
  /**
   * @param int
   */
  public function setTagLength($tagLength)
  {
    $this->tagLength = $tagLength;
  }
  /**
   * @return int
   */
  public function getTagLength()
  {
    return $this->tagLength;
  }
  /**
   * @param bool
   */
  public function setVerifiedAdditionalAuthenticatedDataCrc32c($verifiedAdditionalAuthenticatedDataCrc32c)
  {
    $this->verifiedAdditionalAuthenticatedDataCrc32c = $verifiedAdditionalAuthenticatedDataCrc32c;
  }
  /**
   * @return bool
   */
  public function getVerifiedAdditionalAuthenticatedDataCrc32c()
  {
    return $this->verifiedAdditionalAuthenticatedDataCrc32c;
  }
  /**
   * @param bool
   */
  public function setVerifiedInitializationVectorCrc32c($verifiedInitializationVectorCrc32c)
  {
    $this->verifiedInitializationVectorCrc32c = $verifiedInitializationVectorCrc32c;
  }
  /**
   * @return bool
   */
  public function getVerifiedInitializationVectorCrc32c()
  {
    return $this->verifiedInitializationVectorCrc32c;
  }
  /**
   * @param bool
   */
  public function setVerifiedPlaintextCrc32c($verifiedPlaintextCrc32c)
  {
    $this->verifiedPlaintextCrc32c = $verifiedPlaintextCrc32c;
  }
  /**
   * @return bool
   */
  public function getVerifiedPlaintextCrc32c()
  {
    return $this->verifiedPlaintextCrc32c;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(RawEncryptResponse::class, 'Google_Service_CloudKMS_RawEncryptResponse');
