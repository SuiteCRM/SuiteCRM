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

namespace Google\Service\CertificateAuthorityService;

class NameConstraints extends \Google\Collection
{
  protected $collection_key = 'permittedUris';
  /**
   * @var bool
   */
  public $critical;
  /**
   * @var string[]
   */
  public $excludedDnsNames;
  /**
   * @var string[]
   */
  public $excludedEmailAddresses;
  /**
   * @var string[]
   */
  public $excludedIpRanges;
  /**
   * @var string[]
   */
  public $excludedUris;
  /**
   * @var string[]
   */
  public $permittedDnsNames;
  /**
   * @var string[]
   */
  public $permittedEmailAddresses;
  /**
   * @var string[]
   */
  public $permittedIpRanges;
  /**
   * @var string[]
   */
  public $permittedUris;

  /**
   * @param bool
   */
  public function setCritical($critical)
  {
    $this->critical = $critical;
  }
  /**
   * @return bool
   */
  public function getCritical()
  {
    return $this->critical;
  }
  /**
   * @param string[]
   */
  public function setExcludedDnsNames($excludedDnsNames)
  {
    $this->excludedDnsNames = $excludedDnsNames;
  }
  /**
   * @return string[]
   */
  public function getExcludedDnsNames()
  {
    return $this->excludedDnsNames;
  }
  /**
   * @param string[]
   */
  public function setExcludedEmailAddresses($excludedEmailAddresses)
  {
    $this->excludedEmailAddresses = $excludedEmailAddresses;
  }
  /**
   * @return string[]
   */
  public function getExcludedEmailAddresses()
  {
    return $this->excludedEmailAddresses;
  }
  /**
   * @param string[]
   */
  public function setExcludedIpRanges($excludedIpRanges)
  {
    $this->excludedIpRanges = $excludedIpRanges;
  }
  /**
   * @return string[]
   */
  public function getExcludedIpRanges()
  {
    return $this->excludedIpRanges;
  }
  /**
   * @param string[]
   */
  public function setExcludedUris($excludedUris)
  {
    $this->excludedUris = $excludedUris;
  }
  /**
   * @return string[]
   */
  public function getExcludedUris()
  {
    return $this->excludedUris;
  }
  /**
   * @param string[]
   */
  public function setPermittedDnsNames($permittedDnsNames)
  {
    $this->permittedDnsNames = $permittedDnsNames;
  }
  /**
   * @return string[]
   */
  public function getPermittedDnsNames()
  {
    return $this->permittedDnsNames;
  }
  /**
   * @param string[]
   */
  public function setPermittedEmailAddresses($permittedEmailAddresses)
  {
    $this->permittedEmailAddresses = $permittedEmailAddresses;
  }
  /**
   * @return string[]
   */
  public function getPermittedEmailAddresses()
  {
    return $this->permittedEmailAddresses;
  }
  /**
   * @param string[]
   */
  public function setPermittedIpRanges($permittedIpRanges)
  {
    $this->permittedIpRanges = $permittedIpRanges;
  }
  /**
   * @return string[]
   */
  public function getPermittedIpRanges()
  {
    return $this->permittedIpRanges;
  }
  /**
   * @param string[]
   */
  public function setPermittedUris($permittedUris)
  {
    $this->permittedUris = $permittedUris;
  }
  /**
   * @return string[]
   */
  public function getPermittedUris()
  {
    return $this->permittedUris;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(NameConstraints::class, 'Google_Service_CertificateAuthorityService_NameConstraints');
