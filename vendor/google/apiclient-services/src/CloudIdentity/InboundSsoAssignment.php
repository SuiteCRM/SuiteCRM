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

namespace Google\Service\CloudIdentity;

class InboundSsoAssignment extends \Google\Model
{
  /**
   * @var string
   */
  public $customer;
  /**
   * @var string
   */
  public $name;
  /**
   * @var int
   */
  public $rank;
  protected $samlSsoInfoType = SamlSsoInfo::class;
  protected $samlSsoInfoDataType = '';
  protected $signInBehaviorType = SignInBehavior::class;
  protected $signInBehaviorDataType = '';
  /**
   * @var string
   */
  public $ssoMode;
  /**
   * @var string
   */
  public $targetGroup;
  /**
   * @var string
   */
  public $targetOrgUnit;

  /**
   * @param string
   */
  public function setCustomer($customer)
  {
    $this->customer = $customer;
  }
  /**
   * @return string
   */
  public function getCustomer()
  {
    return $this->customer;
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
   * @param int
   */
  public function setRank($rank)
  {
    $this->rank = $rank;
  }
  /**
   * @return int
   */
  public function getRank()
  {
    return $this->rank;
  }
  /**
   * @param SamlSsoInfo
   */
  public function setSamlSsoInfo(SamlSsoInfo $samlSsoInfo)
  {
    $this->samlSsoInfo = $samlSsoInfo;
  }
  /**
   * @return SamlSsoInfo
   */
  public function getSamlSsoInfo()
  {
    return $this->samlSsoInfo;
  }
  /**
   * @param SignInBehavior
   */
  public function setSignInBehavior(SignInBehavior $signInBehavior)
  {
    $this->signInBehavior = $signInBehavior;
  }
  /**
   * @return SignInBehavior
   */
  public function getSignInBehavior()
  {
    return $this->signInBehavior;
  }
  /**
   * @param string
   */
  public function setSsoMode($ssoMode)
  {
    $this->ssoMode = $ssoMode;
  }
  /**
   * @return string
   */
  public function getSsoMode()
  {
    return $this->ssoMode;
  }
  /**
   * @param string
   */
  public function setTargetGroup($targetGroup)
  {
    $this->targetGroup = $targetGroup;
  }
  /**
   * @return string
   */
  public function getTargetGroup()
  {
    return $this->targetGroup;
  }
  /**
   * @param string
   */
  public function setTargetOrgUnit($targetOrgUnit)
  {
    $this->targetOrgUnit = $targetOrgUnit;
  }
  /**
   * @return string
   */
  public function getTargetOrgUnit()
  {
    return $this->targetOrgUnit;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(InboundSsoAssignment::class, 'Google_Service_CloudIdentity_InboundSsoAssignment');
