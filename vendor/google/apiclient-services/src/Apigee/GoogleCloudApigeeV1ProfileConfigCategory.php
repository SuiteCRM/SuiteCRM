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

namespace Google\Service\Apigee;

class GoogleCloudApigeeV1ProfileConfigCategory extends \Google\Model
{
  protected $abuseType = GoogleCloudApigeeV1ProfileConfigAbuse::class;
  protected $abuseDataType = '';
  protected $authorizationType = GoogleCloudApigeeV1ProfileConfigAuthorization::class;
  protected $authorizationDataType = '';
  protected $corsType = GoogleCloudApigeeV1ProfileConfigCORS::class;
  protected $corsDataType = '';
  protected $mediationType = GoogleCloudApigeeV1ProfileConfigMediation::class;
  protected $mediationDataType = '';
  protected $mtlsType = GoogleCloudApigeeV1ProfileConfigMTLS::class;
  protected $mtlsDataType = '';
  protected $threatType = GoogleCloudApigeeV1ProfileConfigThreat::class;
  protected $threatDataType = '';

  /**
   * @param GoogleCloudApigeeV1ProfileConfigAbuse
   */
  public function setAbuse(GoogleCloudApigeeV1ProfileConfigAbuse $abuse)
  {
    $this->abuse = $abuse;
  }
  /**
   * @return GoogleCloudApigeeV1ProfileConfigAbuse
   */
  public function getAbuse()
  {
    return $this->abuse;
  }
  /**
   * @param GoogleCloudApigeeV1ProfileConfigAuthorization
   */
  public function setAuthorization(GoogleCloudApigeeV1ProfileConfigAuthorization $authorization)
  {
    $this->authorization = $authorization;
  }
  /**
   * @return GoogleCloudApigeeV1ProfileConfigAuthorization
   */
  public function getAuthorization()
  {
    return $this->authorization;
  }
  /**
   * @param GoogleCloudApigeeV1ProfileConfigCORS
   */
  public function setCors(GoogleCloudApigeeV1ProfileConfigCORS $cors)
  {
    $this->cors = $cors;
  }
  /**
   * @return GoogleCloudApigeeV1ProfileConfigCORS
   */
  public function getCors()
  {
    return $this->cors;
  }
  /**
   * @param GoogleCloudApigeeV1ProfileConfigMediation
   */
  public function setMediation(GoogleCloudApigeeV1ProfileConfigMediation $mediation)
  {
    $this->mediation = $mediation;
  }
  /**
   * @return GoogleCloudApigeeV1ProfileConfigMediation
   */
  public function getMediation()
  {
    return $this->mediation;
  }
  /**
   * @param GoogleCloudApigeeV1ProfileConfigMTLS
   */
  public function setMtls(GoogleCloudApigeeV1ProfileConfigMTLS $mtls)
  {
    $this->mtls = $mtls;
  }
  /**
   * @return GoogleCloudApigeeV1ProfileConfigMTLS
   */
  public function getMtls()
  {
    return $this->mtls;
  }
  /**
   * @param GoogleCloudApigeeV1ProfileConfigThreat
   */
  public function setThreat(GoogleCloudApigeeV1ProfileConfigThreat $threat)
  {
    $this->threat = $threat;
  }
  /**
   * @return GoogleCloudApigeeV1ProfileConfigThreat
   */
  public function getThreat()
  {
    return $this->threat;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudApigeeV1ProfileConfigCategory::class, 'Google_Service_Apigee_GoogleCloudApigeeV1ProfileConfigCategory');
