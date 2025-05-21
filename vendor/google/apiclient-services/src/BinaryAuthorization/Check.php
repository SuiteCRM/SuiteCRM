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

namespace Google\Service\BinaryAuthorization;

class Check extends \Google\Model
{
  /**
   * @var bool
   */
  public $alwaysDeny;
  /**
   * @var string
   */
  public $displayName;
  protected $imageAllowlistType = ImageAllowlist::class;
  protected $imageAllowlistDataType = '';
  protected $imageFreshnessCheckType = ImageFreshnessCheck::class;
  protected $imageFreshnessCheckDataType = '';
  protected $sigstoreSignatureCheckType = SigstoreSignatureCheck::class;
  protected $sigstoreSignatureCheckDataType = '';
  protected $simpleSigningAttestationCheckType = SimpleSigningAttestationCheck::class;
  protected $simpleSigningAttestationCheckDataType = '';
  protected $slsaCheckType = SlsaCheck::class;
  protected $slsaCheckDataType = '';
  protected $trustedDirectoryCheckType = TrustedDirectoryCheck::class;
  protected $trustedDirectoryCheckDataType = '';
  protected $vulnerabilityCheckType = VulnerabilityCheck::class;
  protected $vulnerabilityCheckDataType = '';

  /**
   * @param bool
   */
  public function setAlwaysDeny($alwaysDeny)
  {
    $this->alwaysDeny = $alwaysDeny;
  }
  /**
   * @return bool
   */
  public function getAlwaysDeny()
  {
    return $this->alwaysDeny;
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
   * @param ImageAllowlist
   */
  public function setImageAllowlist(ImageAllowlist $imageAllowlist)
  {
    $this->imageAllowlist = $imageAllowlist;
  }
  /**
   * @return ImageAllowlist
   */
  public function getImageAllowlist()
  {
    return $this->imageAllowlist;
  }
  /**
   * @param ImageFreshnessCheck
   */
  public function setImageFreshnessCheck(ImageFreshnessCheck $imageFreshnessCheck)
  {
    $this->imageFreshnessCheck = $imageFreshnessCheck;
  }
  /**
   * @return ImageFreshnessCheck
   */
  public function getImageFreshnessCheck()
  {
    return $this->imageFreshnessCheck;
  }
  /**
   * @param SigstoreSignatureCheck
   */
  public function setSigstoreSignatureCheck(SigstoreSignatureCheck $sigstoreSignatureCheck)
  {
    $this->sigstoreSignatureCheck = $sigstoreSignatureCheck;
  }
  /**
   * @return SigstoreSignatureCheck
   */
  public function getSigstoreSignatureCheck()
  {
    return $this->sigstoreSignatureCheck;
  }
  /**
   * @param SimpleSigningAttestationCheck
   */
  public function setSimpleSigningAttestationCheck(SimpleSigningAttestationCheck $simpleSigningAttestationCheck)
  {
    $this->simpleSigningAttestationCheck = $simpleSigningAttestationCheck;
  }
  /**
   * @return SimpleSigningAttestationCheck
   */
  public function getSimpleSigningAttestationCheck()
  {
    return $this->simpleSigningAttestationCheck;
  }
  /**
   * @param SlsaCheck
   */
  public function setSlsaCheck(SlsaCheck $slsaCheck)
  {
    $this->slsaCheck = $slsaCheck;
  }
  /**
   * @return SlsaCheck
   */
  public function getSlsaCheck()
  {
    return $this->slsaCheck;
  }
  /**
   * @param TrustedDirectoryCheck
   */
  public function setTrustedDirectoryCheck(TrustedDirectoryCheck $trustedDirectoryCheck)
  {
    $this->trustedDirectoryCheck = $trustedDirectoryCheck;
  }
  /**
   * @return TrustedDirectoryCheck
   */
  public function getTrustedDirectoryCheck()
  {
    return $this->trustedDirectoryCheck;
  }
  /**
   * @param VulnerabilityCheck
   */
  public function setVulnerabilityCheck(VulnerabilityCheck $vulnerabilityCheck)
  {
    $this->vulnerabilityCheck = $vulnerabilityCheck;
  }
  /**
   * @return VulnerabilityCheck
   */
  public function getVulnerabilityCheck()
  {
    return $this->vulnerabilityCheck;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(Check::class, 'Google_Service_BinaryAuthorization_Check');
