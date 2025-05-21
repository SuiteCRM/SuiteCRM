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

namespace Google\Service\Compute;

class InterconnectDiagnostics extends \Google\Collection
{
  protected $collection_key = 'links';
  protected $arpCachesType = InterconnectDiagnosticsARPEntry::class;
  protected $arpCachesDataType = 'array';
  /**
   * @var string
   */
  public $bundleAggregationType;
  /**
   * @var string
   */
  public $bundleOperationalStatus;
  protected $linksType = InterconnectDiagnosticsLinkStatus::class;
  protected $linksDataType = 'array';
  /**
   * @var string
   */
  public $macAddress;

  /**
   * @param InterconnectDiagnosticsARPEntry[]
   */
  public function setArpCaches($arpCaches)
  {
    $this->arpCaches = $arpCaches;
  }
  /**
   * @return InterconnectDiagnosticsARPEntry[]
   */
  public function getArpCaches()
  {
    return $this->arpCaches;
  }
  /**
   * @param string
   */
  public function setBundleAggregationType($bundleAggregationType)
  {
    $this->bundleAggregationType = $bundleAggregationType;
  }
  /**
   * @return string
   */
  public function getBundleAggregationType()
  {
    return $this->bundleAggregationType;
  }
  /**
   * @param string
   */
  public function setBundleOperationalStatus($bundleOperationalStatus)
  {
    $this->bundleOperationalStatus = $bundleOperationalStatus;
  }
  /**
   * @return string
   */
  public function getBundleOperationalStatus()
  {
    return $this->bundleOperationalStatus;
  }
  /**
   * @param InterconnectDiagnosticsLinkStatus[]
   */
  public function setLinks($links)
  {
    $this->links = $links;
  }
  /**
   * @return InterconnectDiagnosticsLinkStatus[]
   */
  public function getLinks()
  {
    return $this->links;
  }
  /**
   * @param string
   */
  public function setMacAddress($macAddress)
  {
    $this->macAddress = $macAddress;
  }
  /**
   * @return string
   */
  public function getMacAddress()
  {
    return $this->macAddress;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(InterconnectDiagnostics::class, 'Google_Service_Compute_InterconnectDiagnostics');
