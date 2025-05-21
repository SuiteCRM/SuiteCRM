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

namespace Google\Service\Sasportal;

class SasPortalProvisionDeploymentRequest extends \Google\Model
{
  /**
   * @var string
   */
  public $newDeploymentDisplayName;
  /**
   * @var string
   */
  public $newOrganizationDisplayName;
  /**
   * @var string
   */
  public $organizationId;

  /**
   * @param string
   */
  public function setNewDeploymentDisplayName($newDeploymentDisplayName)
  {
    $this->newDeploymentDisplayName = $newDeploymentDisplayName;
  }
  /**
   * @return string
   */
  public function getNewDeploymentDisplayName()
  {
    return $this->newDeploymentDisplayName;
  }
  /**
   * @param string
   */
  public function setNewOrganizationDisplayName($newOrganizationDisplayName)
  {
    $this->newOrganizationDisplayName = $newOrganizationDisplayName;
  }
  /**
   * @return string
   */
  public function getNewOrganizationDisplayName()
  {
    return $this->newOrganizationDisplayName;
  }
  /**
   * @param string
   */
  public function setOrganizationId($organizationId)
  {
    $this->organizationId = $organizationId;
  }
  /**
   * @return string
   */
  public function getOrganizationId()
  {
    return $this->organizationId;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(SasPortalProvisionDeploymentRequest::class, 'Google_Service_Sasportal_SasPortalProvisionDeploymentRequest');
