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

namespace Google\Service\CloudAsset;

class OrgPolicyResult extends \Google\Collection
{
  protected $collection_key = 'policyBundle';
  protected $consolidatedPolicyType = AnalyzerOrgPolicy::class;
  protected $consolidatedPolicyDataType = '';
  /**
   * @var string[]
   */
  public $folders;
  /**
   * @var string
   */
  public $organization;
  protected $policyBundleType = AnalyzerOrgPolicy::class;
  protected $policyBundleDataType = 'array';
  /**
   * @var string
   */
  public $project;

  /**
   * @param AnalyzerOrgPolicy
   */
  public function setConsolidatedPolicy(AnalyzerOrgPolicy $consolidatedPolicy)
  {
    $this->consolidatedPolicy = $consolidatedPolicy;
  }
  /**
   * @return AnalyzerOrgPolicy
   */
  public function getConsolidatedPolicy()
  {
    return $this->consolidatedPolicy;
  }
  /**
   * @param string[]
   */
  public function setFolders($folders)
  {
    $this->folders = $folders;
  }
  /**
   * @return string[]
   */
  public function getFolders()
  {
    return $this->folders;
  }
  /**
   * @param string
   */
  public function setOrganization($organization)
  {
    $this->organization = $organization;
  }
  /**
   * @return string
   */
  public function getOrganization()
  {
    return $this->organization;
  }
  /**
   * @param AnalyzerOrgPolicy[]
   */
  public function setPolicyBundle($policyBundle)
  {
    $this->policyBundle = $policyBundle;
  }
  /**
   * @return AnalyzerOrgPolicy[]
   */
  public function getPolicyBundle()
  {
    return $this->policyBundle;
  }
  /**
   * @param string
   */
  public function setProject($project)
  {
    $this->project = $project;
  }
  /**
   * @return string
   */
  public function getProject()
  {
    return $this->project;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(OrgPolicyResult::class, 'Google_Service_CloudAsset_OrgPolicyResult');
