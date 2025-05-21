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

namespace Google\Service\Apigee;

class GoogleCloudApigeeV1DeploymentGroupConfig extends \Google\Model
{
  /**
   * @var string
   */
  public $deploymentGroupType;
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $revisionId;
  /**
   * @var string
   */
  public $uid;

  /**
   * @param string
   */
  public function setDeploymentGroupType($deploymentGroupType)
  {
    $this->deploymentGroupType = $deploymentGroupType;
  }
  /**
   * @return string
   */
  public function getDeploymentGroupType()
  {
    return $this->deploymentGroupType;
  }
  /**
   * @param string
   */
  public function setName($name)
  {
    $this->name = $name;
  }
  /**
   * @return string
   */
  public function getName()
  {
    return $this->name;
  }
  /**
   * @param string
   */
  public function setRevisionId($revisionId)
  {
    $this->revisionId = $revisionId;
  }
  /**
   * @return string
   */
  public function getRevisionId()
  {
    return $this->revisionId;
  }
  /**
   * @param string
   */
  public function setUid($uid)
  {
    $this->uid = $uid;
  }
  /**
   * @return string
   */
  public function getUid()
  {
    return $this->uid;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudApigeeV1DeploymentGroupConfig::class, 'Google_Service_Apigee_GoogleCloudApigeeV1DeploymentGroupConfig');
