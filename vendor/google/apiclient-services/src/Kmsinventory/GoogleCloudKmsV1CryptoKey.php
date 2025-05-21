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

namespace Google\Service\Kmsinventory;

class GoogleCloudKmsV1CryptoKey extends \Google\Model
{
  /**
   * @var string
   */
  public $createTime;
  /**
   * @var string
   */
  public $cryptoKeyBackend;
  /**
   * @var string
   */
  public $destroyScheduledDuration;
  /**
   * @var bool
   */
  public $importOnly;
  protected $keyAccessJustificationsPolicyType = GoogleCloudKmsV1KeyAccessJustificationsPolicy::class;
  protected $keyAccessJustificationsPolicyDataType = '';
  /**
   * @var string[]
   */
  public $labels;
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $nextRotationTime;
  protected $primaryType = GoogleCloudKmsV1CryptoKeyVersion::class;
  protected $primaryDataType = '';
  /**
   * @var string
   */
  public $purpose;
  /**
   * @var string
   */
  public $rotationPeriod;
  protected $versionTemplateType = GoogleCloudKmsV1CryptoKeyVersionTemplate::class;
  protected $versionTemplateDataType = '';

  /**
   * @param string
   */
  public function setCreateTime($createTime)
  {
    $this->createTime = $createTime;
  }
  /**
   * @return string
   */
  public function getCreateTime()
  {
    return $this->createTime;
  }
  /**
   * @param string
   */
  public function setCryptoKeyBackend($cryptoKeyBackend)
  {
    $this->cryptoKeyBackend = $cryptoKeyBackend;
  }
  /**
   * @return string
   */
  public function getCryptoKeyBackend()
  {
    return $this->cryptoKeyBackend;
  }
  /**
   * @param string
   */
  public function setDestroyScheduledDuration($destroyScheduledDuration)
  {
    $this->destroyScheduledDuration = $destroyScheduledDuration;
  }
  /**
   * @return string
   */
  public function getDestroyScheduledDuration()
  {
    return $this->destroyScheduledDuration;
  }
  /**
   * @param bool
   */
  public function setImportOnly($importOnly)
  {
    $this->importOnly = $importOnly;
  }
  /**
   * @return bool
   */
  public function getImportOnly()
  {
    return $this->importOnly;
  }
  /**
   * @param GoogleCloudKmsV1KeyAccessJustificationsPolicy
   */
  public function setKeyAccessJustificationsPolicy(GoogleCloudKmsV1KeyAccessJustificationsPolicy $keyAccessJustificationsPolicy)
  {
    $this->keyAccessJustificationsPolicy = $keyAccessJustificationsPolicy;
  }
  /**
   * @return GoogleCloudKmsV1KeyAccessJustificationsPolicy
   */
  public function getKeyAccessJustificationsPolicy()
  {
    return $this->keyAccessJustificationsPolicy;
  }
  /**
   * @param string[]
   */
  public function setLabels($labels)
  {
    $this->labels = $labels;
  }
  /**
   * @return string[]
   */
  public function getLabels()
  {
    return $this->labels;
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
  public function setNextRotationTime($nextRotationTime)
  {
    $this->nextRotationTime = $nextRotationTime;
  }
  /**
   * @return string
   */
  public function getNextRotationTime()
  {
    return $this->nextRotationTime;
  }
  /**
   * @param GoogleCloudKmsV1CryptoKeyVersion
   */
  public function setPrimary(GoogleCloudKmsV1CryptoKeyVersion $primary)
  {
    $this->primary = $primary;
  }
  /**
   * @return GoogleCloudKmsV1CryptoKeyVersion
   */
  public function getPrimary()
  {
    return $this->primary;
  }
  /**
   * @param string
   */
  public function setPurpose($purpose)
  {
    $this->purpose = $purpose;
  }
  /**
   * @return string
   */
  public function getPurpose()
  {
    return $this->purpose;
  }
  /**
   * @param string
   */
  public function setRotationPeriod($rotationPeriod)
  {
    $this->rotationPeriod = $rotationPeriod;
  }
  /**
   * @return string
   */
  public function getRotationPeriod()
  {
    return $this->rotationPeriod;
  }
  /**
   * @param GoogleCloudKmsV1CryptoKeyVersionTemplate
   */
  public function setVersionTemplate(GoogleCloudKmsV1CryptoKeyVersionTemplate $versionTemplate)
  {
    $this->versionTemplate = $versionTemplate;
  }
  /**
   * @return GoogleCloudKmsV1CryptoKeyVersionTemplate
   */
  public function getVersionTemplate()
  {
    return $this->versionTemplate;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudKmsV1CryptoKey::class, 'Google_Service_Kmsinventory_GoogleCloudKmsV1CryptoKey');
