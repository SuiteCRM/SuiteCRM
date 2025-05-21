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

namespace Google\Service\CloudDomains;

class RRSetRoutingPolicy extends \Google\Model
{
  protected $geoType = GeoPolicy::class;
  protected $geoDataType = '';
  protected $geoPolicyType = GeoPolicy::class;
  protected $geoPolicyDataType = '';
  /**
   * @var string
   */
  public $healthCheck;
  protected $primaryBackupType = PrimaryBackupPolicy::class;
  protected $primaryBackupDataType = '';
  protected $wrrType = WrrPolicy::class;
  protected $wrrDataType = '';
  protected $wrrPolicyType = WrrPolicy::class;
  protected $wrrPolicyDataType = '';

  /**
   * @param GeoPolicy
   */
  public function setGeo(GeoPolicy $geo)
  {
    $this->geo = $geo;
  }
  /**
   * @return GeoPolicy
   */
  public function getGeo()
  {
    return $this->geo;
  }
  /**
   * @param GeoPolicy
   */
  public function setGeoPolicy(GeoPolicy $geoPolicy)
  {
    $this->geoPolicy = $geoPolicy;
  }
  /**
   * @return GeoPolicy
   */
  public function getGeoPolicy()
  {
    return $this->geoPolicy;
  }
  /**
   * @param string
   */
  public function setHealthCheck($healthCheck)
  {
    $this->healthCheck = $healthCheck;
  }
  /**
   * @return string
   */
  public function getHealthCheck()
  {
    return $this->healthCheck;
  }
  /**
   * @param PrimaryBackupPolicy
   */
  public function setPrimaryBackup(PrimaryBackupPolicy $primaryBackup)
  {
    $this->primaryBackup = $primaryBackup;
  }
  /**
   * @return PrimaryBackupPolicy
   */
  public function getPrimaryBackup()
  {
    return $this->primaryBackup;
  }
  /**
   * @param WrrPolicy
   */
  public function setWrr(WrrPolicy $wrr)
  {
    $this->wrr = $wrr;
  }
  /**
   * @return WrrPolicy
   */
  public function getWrr()
  {
    return $this->wrr;
  }
  /**
   * @param WrrPolicy
   */
  public function setWrrPolicy(WrrPolicy $wrrPolicy)
  {
    $this->wrrPolicy = $wrrPolicy;
  }
  /**
   * @return WrrPolicy
   */
  public function getWrrPolicy()
  {
    return $this->wrrPolicy;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(RRSetRoutingPolicy::class, 'Google_Service_CloudDomains_RRSetRoutingPolicy');
