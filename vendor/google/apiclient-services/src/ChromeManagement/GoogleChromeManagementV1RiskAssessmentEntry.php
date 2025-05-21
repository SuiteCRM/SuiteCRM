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

namespace Google\Service\ChromeManagement;

class GoogleChromeManagementV1RiskAssessmentEntry extends \Google\Model
{
  /**
   * @var string
   */
  public $provider;
  protected $riskAssessmentType = GoogleChromeManagementV1RiskAssessment::class;
  protected $riskAssessmentDataType = '';
  /**
   * @var string
   */
  public $riskLevel;

  /**
   * @param string
   */
  public function setProvider($provider)
  {
    $this->provider = $provider;
  }
  /**
   * @return string
   */
  public function getProvider()
  {
    return $this->provider;
  }
  /**
   * @param GoogleChromeManagementV1RiskAssessment
   */
  public function setRiskAssessment(GoogleChromeManagementV1RiskAssessment $riskAssessment)
  {
    $this->riskAssessment = $riskAssessment;
  }
  /**
   * @return GoogleChromeManagementV1RiskAssessment
   */
  public function getRiskAssessment()
  {
    return $this->riskAssessment;
  }
  /**
   * @param string
   */
  public function setRiskLevel($riskLevel)
  {
    $this->riskLevel = $riskLevel;
  }
  /**
   * @return string
   */
  public function getRiskLevel()
  {
    return $this->riskLevel;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleChromeManagementV1RiskAssessmentEntry::class, 'Google_Service_ChromeManagement_GoogleChromeManagementV1RiskAssessmentEntry');
