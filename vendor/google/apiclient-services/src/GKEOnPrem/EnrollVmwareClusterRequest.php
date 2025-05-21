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

class EnrollVmwareClusterRequest extends \Google\Model
{
  /**
   * @var string
   */
  public $adminClusterMembership;
  /**
   * @var string
   */
  public $localName;
  /**
   * @var bool
   */
  public $validateOnly;
  /**
   * @var string
   */
  public $vmwareClusterId;

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
  /**
   * @param bool
   */
  public function setValidateOnly($validateOnly)
  {
    $this->validateOnly = $validateOnly;
  }
  /**
   * @return bool
   */
  public function getValidateOnly()
  {
    return $this->validateOnly;
  }
  /**
   * @param string
   */
  public function setVmwareClusterId($vmwareClusterId)
  {
    $this->vmwareClusterId = $vmwareClusterId;
  }
  /**
   * @return string
   */
  public function getVmwareClusterId()
  {
    return $this->vmwareClusterId;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(EnrollVmwareClusterRequest::class, 'Google_Service_GKEOnPrem_EnrollVmwareClusterRequest');
