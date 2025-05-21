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

namespace Google\Service\RecaptchaEnterprise;

class GoogleCloudRecaptchaenterpriseV1FraudPreventionAssessment extends \Google\Model
{
  protected $behavioralTrustVerdictType = GoogleCloudRecaptchaenterpriseV1FraudPreventionAssessmentBehavioralTrustVerdict::class;
  protected $behavioralTrustVerdictDataType = '';
  protected $cardTestingVerdictType = GoogleCloudRecaptchaenterpriseV1FraudPreventionAssessmentCardTestingVerdict::class;
  protected $cardTestingVerdictDataType = '';
  protected $stolenInstrumentVerdictType = GoogleCloudRecaptchaenterpriseV1FraudPreventionAssessmentStolenInstrumentVerdict::class;
  protected $stolenInstrumentVerdictDataType = '';
  /**
   * @var float
   */
  public $transactionRisk;

  /**
   * @param GoogleCloudRecaptchaenterpriseV1FraudPreventionAssessmentBehavioralTrustVerdict
   */
  public function setBehavioralTrustVerdict(GoogleCloudRecaptchaenterpriseV1FraudPreventionAssessmentBehavioralTrustVerdict $behavioralTrustVerdict)
  {
    $this->behavioralTrustVerdict = $behavioralTrustVerdict;
  }
  /**
   * @return GoogleCloudRecaptchaenterpriseV1FraudPreventionAssessmentBehavioralTrustVerdict
   */
  public function getBehavioralTrustVerdict()
  {
    return $this->behavioralTrustVerdict;
  }
  /**
   * @param GoogleCloudRecaptchaenterpriseV1FraudPreventionAssessmentCardTestingVerdict
   */
  public function setCardTestingVerdict(GoogleCloudRecaptchaenterpriseV1FraudPreventionAssessmentCardTestingVerdict $cardTestingVerdict)
  {
    $this->cardTestingVerdict = $cardTestingVerdict;
  }
  /**
   * @return GoogleCloudRecaptchaenterpriseV1FraudPreventionAssessmentCardTestingVerdict
   */
  public function getCardTestingVerdict()
  {
    return $this->cardTestingVerdict;
  }
  /**
   * @param GoogleCloudRecaptchaenterpriseV1FraudPreventionAssessmentStolenInstrumentVerdict
   */
  public function setStolenInstrumentVerdict(GoogleCloudRecaptchaenterpriseV1FraudPreventionAssessmentStolenInstrumentVerdict $stolenInstrumentVerdict)
  {
    $this->stolenInstrumentVerdict = $stolenInstrumentVerdict;
  }
  /**
   * @return GoogleCloudRecaptchaenterpriseV1FraudPreventionAssessmentStolenInstrumentVerdict
   */
  public function getStolenInstrumentVerdict()
  {
    return $this->stolenInstrumentVerdict;
  }
  /**
   * @param float
   */
  public function setTransactionRisk($transactionRisk)
  {
    $this->transactionRisk = $transactionRisk;
  }
  /**
   * @return float
   */
  public function getTransactionRisk()
  {
    return $this->transactionRisk;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudRecaptchaenterpriseV1FraudPreventionAssessment::class, 'Google_Service_RecaptchaEnterprise_GoogleCloudRecaptchaenterpriseV1FraudPreventionAssessment');
