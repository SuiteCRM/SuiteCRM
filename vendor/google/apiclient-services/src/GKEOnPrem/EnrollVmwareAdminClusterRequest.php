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

class EnrollVmwareAdminClusterRequest extends \Google\Model
{
  /**
   * @var string
   */
  public $membership;
  /**
   * @var string
   */
  public $vmwareAdminClusterId;

  /**
   * @param string
   */
  public function setMembership($membership)
  {
    $this->membership = $membership;
  }
  /**
   * @return string
   */
  public function getMembership()
  {
    return $this->membership;
  }
  /**
   * @param string
   */
  public function setVmwareAdminClusterId($vmwareAdminClusterId)
  {
    $this->vmwareAdminClusterId = $vmwareAdminClusterId;
  }
  /**
   * @return string
   */
  public function getVmwareAdminClusterId()
  {
    return $this->vmwareAdminClusterId;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(EnrollVmwareAdminClusterRequest::class, 'Google_Service_GKEOnPrem_EnrollVmwareAdminClusterRequest');
