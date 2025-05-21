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

class EnrollBareMetalClusterRequest extends \Google\Model
{
  /**
   * @var string
   */
  public $adminClusterMembership;
  /**
   * @var string
   */
  public $bareMetalClusterId;
  /**
   * @var string
   */
  public $localName;

  /**
   * @param string
   */
  public function setAdminClusterMembership($adminClusterMembership)
  {
    $this->adminClusterMembership = $adminClusterMembership;
  }
  /**
   * @return string
   */
  public function getAdminClusterMembership()
  {
    return $this->adminClusterMembership;
  }
  /**
   * @param string
   */
  public function setBareMetalClusterId($bareMetalClusterId)
  {
    $this->bareMetalClusterId = $bareMetalClusterId;
  }
  /**
   * @return string
   */
  public function getBareMetalClusterId()
  {
    return $this->bareMetalClusterId;
  }
  /**
   * @param string
   */
  public function setLocalName($localName)
  {
    $this->localName = $localName;
  }
  /**
   * @return string
   */
  public function getLocalName()
  {
    return $this->localName;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(EnrollBareMetalClusterRequest::class, 'Google_Service_GKEOnPrem_EnrollBareMetalClusterRequest');
