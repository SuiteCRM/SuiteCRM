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

namespace Google\Service\GKEHub;

class IdentityServiceSamlConfig extends \Google\Collection
{
  protected $collection_key = 'identityProviderCertificates';
  /**
   * @var string[]
   */
  public $attributeMapping;
  /**
   * @var string
   */
  public $groupPrefix;
  /**
   * @var string
   */
  public $groupsAttribute;
  /**
   * @var string[]
   */
  public $identityProviderCertificates;
  /**
   * @var string
   */
  public $identityProviderId;
  /**
   * @var string
   */
  public $identityProviderSsoUri;
  /**
   * @var string
   */
  public $userAttribute;
  /**
   * @var string
   */
  public $userPrefix;

  /**
   * @param string[]
   */
  public function setAttributeMapping($attributeMapping)
  {
    $this->attributeMapping = $attributeMapping;
  }
  /**
   * @return string[]
   */
  public function getAttributeMapping()
  {
    return $this->attributeMapping;
  }
  /**
   * @param string
   */
  public function setGroupPrefix($groupPrefix)
  {
    $this->groupPrefix = $groupPrefix;
  }
  /**
   * @return string
   */
  public function getGroupPrefix()
  {
    return $this->groupPrefix;
  }
  /**
   * @param string
   */
  public function setGroupsAttribute($groupsAttribute)
  {
    $this->groupsAttribute = $groupsAttribute;
  }
  /**
   * @return string
   */
  public function getGroupsAttribute()
  {
    return $this->groupsAttribute;
  }
  /**
   * @param string[]
   */
  public function setIdentityProviderCertificates($identityProviderCertificates)
  {
    $this->identityProviderCertificates = $identityProviderCertificates;
  }
  /**
   * @return string[]
   */
  public function getIdentityProviderCertificates()
  {
    return $this->identityProviderCertificates;
  }
  /**
   * @param string
   */
  public function setIdentityProviderId($identityProviderId)
  {
    $this->identityProviderId = $identityProviderId;
  }
  /**
   * @return string
   */
  public function getIdentityProviderId()
  {
    return $this->identityProviderId;
  }
  /**
   * @param string
   */
  public function setIdentityProviderSsoUri($identityProviderSsoUri)
  {
    $this->identityProviderSsoUri = $identityProviderSsoUri;
  }
  /**
   * @return string
   */
  public function getIdentityProviderSsoUri()
  {
    return $this->identityProviderSsoUri;
  }
  /**
   * @param string
   */
  public function setUserAttribute($userAttribute)
  {
    $this->userAttribute = $userAttribute;
  }
  /**
   * @return string
   */
  public function getUserAttribute()
  {
    return $this->userAttribute;
  }
  /**
   * @param string
   */
  public function setUserPrefix($userPrefix)
  {
    $this->userPrefix = $userPrefix;
  }
  /**
   * @return string
   */
  public function getUserPrefix()
  {
    return $this->userPrefix;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(IdentityServiceSamlConfig::class, 'Google_Service_GKEHub_IdentityServiceSamlConfig');
