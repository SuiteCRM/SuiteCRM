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

namespace Google\Service\CloudHealthcare;

class ApplyConsentsResponse extends \Google\Model
{
  /**
   * @var string
   */
  public $affectedResources;
  /**
   * @var string
   */
  public $consentApplyFailure;
  /**
   * @var string
   */
  public $consentApplySuccess;
  /**
   * @var string
   */
  public $failedResources;

  /**
   * @param string
   */
  public function setAffectedResources($affectedResources)
  {
    $this->affectedResources = $affectedResources;
  }
  /**
   * @return string
   */
  public function getAffectedResources()
  {
    return $this->affectedResources;
  }
  /**
   * @param string
   */
  public function setConsentApplyFailure($consentApplyFailure)
  {
    $this->consentApplyFailure = $consentApplyFailure;
  }
  /**
   * @return string
   */
  public function getConsentApplyFailure()
  {
    return $this->consentApplyFailure;
  }
  /**
   * @param string
   */
  public function setConsentApplySuccess($consentApplySuccess)
  {
    $this->consentApplySuccess = $consentApplySuccess;
  }
  /**
   * @return string
   */
  public function getConsentApplySuccess()
  {
    return $this->consentApplySuccess;
  }
  /**
   * @param string
   */
  public function setFailedResources($failedResources)
  {
    $this->failedResources = $failedResources;
  }
  /**
   * @return string
   */
  public function getFailedResources()
  {
    return $this->failedResources;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(ApplyConsentsResponse::class, 'Google_Service_CloudHealthcare_ApplyConsentsResponse');
