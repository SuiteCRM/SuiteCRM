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

namespace Google\Service\PolicySimulator;

class GoogleCloudPolicysimulatorV1OrgPolicyOverlay extends \Google\Collection
{
  protected $collection_key = 'policies';
  protected $customConstraintsType = GoogleCloudPolicysimulatorV1OrgPolicyOverlayCustomConstraintOverlay::class;
  protected $customConstraintsDataType = 'array';
  protected $policiesType = GoogleCloudPolicysimulatorV1OrgPolicyOverlayPolicyOverlay::class;
  protected $policiesDataType = 'array';

  /**
   * @param GoogleCloudPolicysimulatorV1OrgPolicyOverlayCustomConstraintOverlay[]
   */
  public function setCustomConstraints($customConstraints)
  {
    $this->customConstraints = $customConstraints;
  }
  /**
   * @return GoogleCloudPolicysimulatorV1OrgPolicyOverlayCustomConstraintOverlay[]
   */
  public function getCustomConstraints()
  {
    return $this->customConstraints;
  }
  /**
   * @param GoogleCloudPolicysimulatorV1OrgPolicyOverlayPolicyOverlay[]
   */
  public function setPolicies($policies)
  {
    $this->policies = $policies;
  }
  /**
   * @return GoogleCloudPolicysimulatorV1OrgPolicyOverlayPolicyOverlay[]
   */
  public function getPolicies()
  {
    return $this->policies;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudPolicysimulatorV1OrgPolicyOverlay::class, 'Google_Service_PolicySimulator_GoogleCloudPolicysimulatorV1OrgPolicyOverlay');
