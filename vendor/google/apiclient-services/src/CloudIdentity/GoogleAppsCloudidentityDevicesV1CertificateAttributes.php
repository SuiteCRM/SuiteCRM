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

class GoogleAppsCloudidentityDevicesV1CertificateAttributes extends \Google\Model
{
  protected $certificateTemplateType = GoogleAppsCloudidentityDevicesV1CertificateTemplate::class;
  protected $certificateTemplateDataType = '';
  /**
   * @var string
   */
  public $fingerprint;
  /**
   * @var string
   */
  public $issuer;
  /**
   * @var string
   */
  public $serialNumber;
  /**
   * @var string
   */
  public $subject;
  /**
   * @var string
   */
  public $thumbprint;
  /**
   * @var string
   */
  public $validationState;
  /**
   * @var string
   */
  public $validityExpirationTime;
  /**
   * @var string
   */
  public $validityStartTime;

  /**
   * @param GoogleAppsCloudidentityDevicesV1CertificateTemplate
   */
  public function setCertificateTemplate(GoogleAppsCloudidentityDevicesV1CertificateTemplate $certificateTemplate)
  {
    $this->certificateTemplate = $certificateTemplate;
  }
  /**
   * @return GoogleAppsCloudidentityDevicesV1CertificateTemplate
   */
  public function getCertificateTemplate()
  {
    return $this->certificateTemplate;
  }
  /**
   * @param string
   */
  public function setFingerprint($fingerprint)
  {
    $this->fingerprint = $fingerprint;
  }
  /**
   * @return string
   */
  public function getFingerprint()
  {
    return $this->fingerprint;
  }
  /**
   * @param string
   */
  public function setIssuer($issuer)
  {
    $this->issuer = $issuer;
  }
  /**
   * @return string
   */
  public function getIssuer()
  {
    return $this->issuer;
  }
  /**
   * @param string
   */
  public function setSerialNumber($serialNumber)
  {
    $this->serialNumber = $serialNumber;
  }
  /**
   * @return string
   */
  public function getSerialNumber()
  {
    return $this->serialNumber;
  }
  /**
   * @param string
   */
  public function setSubject($subject)
  {
    $this->subject = $subject;
  }
  /**
   * @return string
   */
  public function getSubject()
  {
    return $this->subject;
  }
  /**
   * @param string
   */
  public function setThumbprint($thumbprint)
  {
    $this->thumbprint = $thumbprint;
  }
  /**
   * @return string
   */
  public function getThumbprint()
  {
    return $this->thumbprint;
  }
  /**
   * @param string
   */
  public function setValidationState($validationState)
  {
    $this->validationState = $validationState;
  }
  /**
   * @return string
   */
  public function getValidationState()
  {
    return $this->validationState;
  }
  /**
   * @param string
   */
  public function setValidityExpirationTime($validityExpirationTime)
  {
    $this->validityExpirationTime = $validityExpirationTime;
  }
  /**
   * @return string
   */
  public function getValidityExpirationTime()
  {
    return $this->validityExpirationTime;
  }
  /**
   * @param string
   */
  public function setValidityStartTime($validityStartTime)
  {
    $this->validityStartTime = $validityStartTime;
  }
  /**
   * @return string
   */
  public function getValidityStartTime()
  {
    return $this->validityStartTime;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleAppsCloudidentityDevicesV1CertificateAttributes::class, 'Google_Service_CloudIdentity_GoogleAppsCloudidentityDevicesV1CertificateAttributes');
