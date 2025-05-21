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

namespace Google\Service\CCAIPlatform;

class ContactCenter extends \Google\Collection
{
  protected $collection_key = 'privateComponents';
  protected $adminUserType = AdminUser::class;
  protected $adminUserDataType = '';
  /**
   * @var bool
   */
  public $ccaipManagedUsers;
  /**
   * @var string
   */
  public $createTime;
  protected $criticalType = Critical::class;
  protected $criticalDataType = '';
  /**
   * @var string
   */
  public $customerDomainPrefix;
  /**
   * @var string
   */
  public $displayName;
  protected $earlyType = Early::class;
  protected $earlyDataType = '';
  protected $instanceConfigType = InstanceConfig::class;
  protected $instanceConfigDataType = '';
  /**
   * @var string
   */
  public $kmsKey;
  /**
   * @var string[]
   */
  public $labels;
  /**
   * @var string
   */
  public $name;
  protected $normalType = Normal::class;
  protected $normalDataType = '';
  protected $privateAccessType = PrivateAccess::class;
  protected $privateAccessDataType = '';
  /**
   * @var string[]
   */
  public $privateComponents;
  protected $samlParamsType = SAMLParams::class;
  protected $samlParamsDataType = '';
  /**
   * @var string
   */
  public $state;
  /**
   * @var string
   */
  public $updateTime;
  protected $urisType = URIs::class;
  protected $urisDataType = '';
  /**
   * @var string
   */
  public $userEmail;

  /**
   * @param AdminUser
   */
  public function setAdminUser(AdminUser $adminUser)
  {
    $this->adminUser = $adminUser;
  }
  /**
   * @return AdminUser
   */
  public function getAdminUser()
  {
    return $this->adminUser;
  }
  /**
   * @param bool
   */
  public function setCcaipManagedUsers($ccaipManagedUsers)
  {
    $this->ccaipManagedUsers = $ccaipManagedUsers;
  }
  /**
   * @return bool
   */
  public function getCcaipManagedUsers()
  {
    return $this->ccaipManagedUsers;
  }
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
   * @param Critical
   */
  public function setCritical(Critical $critical)
  {
    $this->critical = $critical;
  }
  /**
   * @return Critical
   */
  public function getCritical()
  {
    return $this->critical;
  }
  /**
   * @param string
   */
  public function setCustomerDomainPrefix($customerDomainPrefix)
  {
    $this->customerDomainPrefix = $customerDomainPrefix;
  }
  /**
   * @return string
   */
  public function getCustomerDomainPrefix()
  {
    return $this->customerDomainPrefix;
  }
  /**
   * @param string
   */
  public function setDisplayName($displayName)
  {
    $this->displayName = $displayName;
  }
  /**
   * @return string
   */
  public function getDisplayName()
  {
    return $this->displayName;
  }
  /**
   * @param Early
   */
  public function setEarly(Early $early)
  {
    $this->early = $early;
  }
  /**
   * @return Early
   */
  public function getEarly()
  {
    return $this->early;
  }
  /**
   * @param InstanceConfig
   */
  public function setInstanceConfig(InstanceConfig $instanceConfig)
  {
    $this->instanceConfig = $instanceConfig;
  }
  /**
   * @return InstanceConfig
   */
  public function getInstanceConfig()
  {
    return $this->instanceConfig;
  }
  /**
   * @param string
   */
  public function setKmsKey($kmsKey)
  {
    $this->kmsKey = $kmsKey;
  }
  /**
   * @return string
   */
  public function getKmsKey()
  {
    return $this->kmsKey;
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
   * @param Normal
   */
  public function setNormal(Normal $normal)
  {
    $this->normal = $normal;
  }
  /**
   * @return Normal
   */
  public function getNormal()
  {
    return $this->normal;
  }
  /**
   * @param PrivateAccess
   */
  public function setPrivateAccess(PrivateAccess $privateAccess)
  {
    $this->privateAccess = $privateAccess;
  }
  /**
   * @return PrivateAccess
   */
  public function getPrivateAccess()
  {
    return $this->privateAccess;
  }
  /**
   * @param string[]
   */
  public function setPrivateComponents($privateComponents)
  {
    $this->privateComponents = $privateComponents;
  }
  /**
   * @return string[]
   */
  public function getPrivateComponents()
  {
    return $this->privateComponents;
  }
  /**
   * @param SAMLParams
   */
  public function setSamlParams(SAMLParams $samlParams)
  {
    $this->samlParams = $samlParams;
  }
  /**
   * @return SAMLParams
   */
  public function getSamlParams()
  {
    return $this->samlParams;
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
  /**
   * @param string
   */
  public function setUpdateTime($updateTime)
  {
    $this->updateTime = $updateTime;
  }
  /**
   * @return string
   */
  public function getUpdateTime()
  {
    return $this->updateTime;
  }
  /**
   * @param URIs
   */
  public function setUris(URIs $uris)
  {
    $this->uris = $uris;
  }
  /**
   * @return URIs
   */
  public function getUris()
  {
    return $this->uris;
  }
  /**
   * @param string
   */
  public function setUserEmail($userEmail)
  {
    $this->userEmail = $userEmail;
  }
  /**
   * @return string
   */
  public function getUserEmail()
  {
    return $this->userEmail;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(ContactCenter::class, 'Google_Service_CCAIPlatform_ContactCenter');
