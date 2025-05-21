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

namespace Google\Service\SecurityCommandCenter;

class CloudArmor extends \Google\Model
{
  protected $adaptiveProtectionType = AdaptiveProtection::class;
  protected $adaptiveProtectionDataType = '';
  protected $attackType = Attack::class;
  protected $attackDataType = '';
  /**
   * @var string
   */
  public $duration;
  protected $requestsType = Requests::class;
  protected $requestsDataType = '';
  protected $securityPolicyType = SecurityPolicy::class;
  protected $securityPolicyDataType = '';
  /**
   * @var string
   */
  public $threatVector;

  /**
   * @param AdaptiveProtection
   */
  public function setAdaptiveProtection(AdaptiveProtection $adaptiveProtection)
  {
    $this->adaptiveProtection = $adaptiveProtection;
  }
  /**
   * @return AdaptiveProtection
   */
  public function getAdaptiveProtection()
  {
    return $this->adaptiveProtection;
  }
  /**
   * @param Attack
   */
  public function setAttack(Attack $attack)
  {
    $this->attack = $attack;
  }
  /**
   * @return Attack
   */
  public function getAttack()
  {
    return $this->attack;
  }
  /**
   * @param string
   */
  public function setDuration($duration)
  {
    $this->duration = $duration;
  }
  /**
   * @return string
   */
  public function getDuration()
  {
    return $this->duration;
  }
  /**
   * @param Requests
   */
  public function setRequests(Requests $requests)
  {
    $this->requests = $requests;
  }
  /**
   * @return Requests
   */
  public function getRequests()
  {
    return $this->requests;
  }
  /**
   * @param SecurityPolicy
   */
  public function setSecurityPolicy(SecurityPolicy $securityPolicy)
  {
    $this->securityPolicy = $securityPolicy;
  }
  /**
   * @return SecurityPolicy
   */
  public function getSecurityPolicy()
  {
    return $this->securityPolicy;
  }
  /**
   * @param string
   */
  public function setThreatVector($threatVector)
  {
    $this->threatVector = $threatVector;
  }
  /**
   * @return string
   */
  public function getThreatVector()
  {
    return $this->threatVector;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(CloudArmor::class, 'Google_Service_SecurityCommandCenter_CloudArmor');
