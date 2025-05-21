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

namespace Google\Service\CloudWorkstations;

class PrivateClusterConfig extends \Google\Collection
{
  protected $collection_key = 'allowedProjects';
  /**
   * @var string[]
   */
  public $allowedProjects;
  /**
   * @var string
   */
  public $clusterHostname;
  /**
   * @var bool
   */
  public $enablePrivateEndpoint;
  /**
   * @var string
   */
  public $serviceAttachmentUri;

  /**
   * @param string[]
   */
  public function setAllowedProjects($allowedProjects)
  {
    $this->allowedProjects = $allowedProjects;
  }
  /**
   * @return string[]
   */
  public function getAllowedProjects()
  {
    return $this->allowedProjects;
  }
  /**
   * @param string
   */
  public function setClusterHostname($clusterHostname)
  {
    $this->clusterHostname = $clusterHostname;
  }
  /**
   * @return string
   */
  public function getClusterHostname()
  {
    return $this->clusterHostname;
  }
  /**
   * @param bool
   */
  public function setEnablePrivateEndpoint($enablePrivateEndpoint)
  {
    $this->enablePrivateEndpoint = $enablePrivateEndpoint;
  }
  /**
   * @return bool
   */
  public function getEnablePrivateEndpoint()
  {
    return $this->enablePrivateEndpoint;
  }
  /**
   * @param string
   */
  public function setServiceAttachmentUri($serviceAttachmentUri)
  {
    $this->serviceAttachmentUri = $serviceAttachmentUri;
  }
  /**
   * @return string
   */
  public function getServiceAttachmentUri()
  {
    return $this->serviceAttachmentUri;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(PrivateClusterConfig::class, 'Google_Service_CloudWorkstations_PrivateClusterConfig');
