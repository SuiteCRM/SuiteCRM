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

namespace Google\Service\GKEOnPrem;

class VmwareHostConfig extends \Google\Collection
{
  protected $collection_key = 'ntpServers';
  /**
   * @var string[]
   */
  public $dnsSearchDomains;
  /**
   * @var string[]
   */
  public $dnsServers;
  /**
   * @var string[]
   */
  public $ntpServers;

  /**
   * @param string[]
   */
  public function setDnsSearchDomains($dnsSearchDomains)
  {
    $this->dnsSearchDomains = $dnsSearchDomains;
  }
  /**
   * @return string[]
   */
  public function getDnsSearchDomains()
  {
    return $this->dnsSearchDomains;
  }
  /**
   * @param string[]
   */
  public function setDnsServers($dnsServers)
  {
    $this->dnsServers = $dnsServers;
  }
  /**
   * @return string[]
   */
  public function getDnsServers()
  {
    return $this->dnsServers;
  }
  /**
   * @param string[]
   */
  public function setNtpServers($ntpServers)
  {
    $this->ntpServers = $ntpServers;
  }
  /**
   * @return string[]
   */
  public function getNtpServers()
  {
    return $this->ntpServers;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(VmwareHostConfig::class, 'Google_Service_GKEOnPrem_VmwareHostConfig');
