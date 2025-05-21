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

namespace Google\Service\AccessContextManager;

class SupportedService extends \Google\Collection
{
  protected $collection_key = 'supportedMethods';
  /**
   * @var bool
   */
  public $availableOnRestrictedVip;
  /**
   * @var bool
   */
  public $knownLimitations;
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $serviceSupportStage;
  /**
   * @var string
   */
  public $supportStage;
  protected $supportedMethodsType = MethodSelector::class;
  protected $supportedMethodsDataType = 'array';
  /**
   * @var string
   */
  public $title;

  /**
   * @param bool
   */
  public function setAvailableOnRestrictedVip($availableOnRestrictedVip)
  {
    $this->availableOnRestrictedVip = $availableOnRestrictedVip;
  }
  /**
   * @return bool
   */
  public function getAvailableOnRestrictedVip()
  {
    return $this->availableOnRestrictedVip;
  }
  /**
   * @param bool
   */
  public function setKnownLimitations($knownLimitations)
  {
    $this->knownLimitations = $knownLimitations;
  }
  /**
   * @return bool
   */
  public function getKnownLimitations()
  {
    return $this->knownLimitations;
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
  public function setServiceSupportStage($serviceSupportStage)
  {
    $this->serviceSupportStage = $serviceSupportStage;
  }
  /**
   * @return string
   */
  public function getServiceSupportStage()
  {
    return $this->serviceSupportStage;
  }
  /**
   * @param string
   */
  public function setSupportStage($supportStage)
  {
    $this->supportStage = $supportStage;
  }
  /**
   * @return string
   */
  public function getSupportStage()
  {
    return $this->supportStage;
  }
  /**
   * @param MethodSelector[]
   */
  public function setSupportedMethods($supportedMethods)
  {
    $this->supportedMethods = $supportedMethods;
  }
  /**
   * @return MethodSelector[]
   */
  public function getSupportedMethods()
  {
    return $this->supportedMethods;
  }
  /**
   * @param string
   */
  public function setTitle($title)
  {
    $this->title = $title;
  }
  /**
   * @return string
   */
  public function getTitle()
  {
    return $this->title;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(SupportedService::class, 'Google_Service_AccessContextManager_SupportedService');
