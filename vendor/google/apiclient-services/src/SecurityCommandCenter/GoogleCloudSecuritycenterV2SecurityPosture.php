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

namespace Google\Service\SecurityCommandCenter;

class GoogleCloudSecuritycenterV2SecurityPosture extends \Google\Collection
{
  protected $collection_key = 'policyDriftDetails';
  /**
   * @var string
   */
  public $changedPolicy;
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $policy;
  protected $policyDriftDetailsType = GoogleCloudSecuritycenterV2PolicyDriftDetails::class;
  protected $policyDriftDetailsDataType = 'array';
  /**
   * @var string
   */
  public $policySet;
  /**
   * @var string
   */
  public $postureDeployment;
  /**
   * @var string
   */
  public $postureDeploymentResource;
  /**
   * @var string
   */
  public $revisionId;

  /**
   * @param string
   */
  public function setChangedPolicy($changedPolicy)
  {
    $this->changedPolicy = $changedPolicy;
  }
  /**
   * @return string
   */
  public function getChangedPolicy()
  {
    return $this->changedPolicy;
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
  public function setPolicy($policy)
  {
    $this->policy = $policy;
  }
  /**
   * @return string
   */
  public function getPolicy()
  {
    return $this->policy;
  }
  /**
   * @param GoogleCloudSecuritycenterV2PolicyDriftDetails[]
   */
  public function setPolicyDriftDetails($policyDriftDetails)
  {
    $this->policyDriftDetails = $policyDriftDetails;
  }
  /**
   * @return GoogleCloudSecuritycenterV2PolicyDriftDetails[]
   */
  public function getPolicyDriftDetails()
  {
    return $this->policyDriftDetails;
  }
  /**
   * @param string
   */
  public function setPolicySet($policySet)
  {
    $this->policySet = $policySet;
  }
  /**
   * @return string
   */
  public function getPolicySet()
  {
    return $this->policySet;
  }
  /**
   * @param string
   */
  public function setPostureDeployment($postureDeployment)
  {
    $this->postureDeployment = $postureDeployment;
  }
  /**
   * @return string
   */
  public function getPostureDeployment()
  {
    return $this->postureDeployment;
  }
  /**
   * @param string
   */
  public function setPostureDeploymentResource($postureDeploymentResource)
  {
    $this->postureDeploymentResource = $postureDeploymentResource;
  }
  /**
   * @return string
   */
  public function getPostureDeploymentResource()
  {
    return $this->postureDeploymentResource;
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
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudSecuritycenterV2SecurityPosture::class, 'Google_Service_SecurityCommandCenter_GoogleCloudSecuritycenterV2SecurityPosture');
