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

class GoogleCloudAssetV1AnalyzeOrgPolicyGovernedAssetsResponseGovernedAsset extends \Google\Collection
{
  protected $collection_key = 'policyBundle';
  protected $consolidatedPolicyType = AnalyzerOrgPolicy::class;
  protected $consolidatedPolicyDataType = '';
  protected $governedIamPolicyType = GoogleCloudAssetV1AnalyzeOrgPolicyGovernedAssetsResponseGovernedIamPolicy::class;
  protected $governedIamPolicyDataType = '';
  protected $governedResourceType = GoogleCloudAssetV1AnalyzeOrgPolicyGovernedAssetsResponseGovernedResource::class;
  protected $governedResourceDataType = '';
  protected $policyBundleType = AnalyzerOrgPolicy::class;
  protected $policyBundleDataType = 'array';

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
   * @param GoogleCloudAssetV1AnalyzeOrgPolicyGovernedAssetsResponseGovernedIamPolicy
   */
  public function setGovernedIamPolicy(GoogleCloudAssetV1AnalyzeOrgPolicyGovernedAssetsResponseGovernedIamPolicy $governedIamPolicy)
  {
    $this->governedIamPolicy = $governedIamPolicy;
  }
  /**
   * @return GoogleCloudAssetV1AnalyzeOrgPolicyGovernedAssetsResponseGovernedIamPolicy
   */
  public function getGovernedIamPolicy()
  {
    return $this->governedIamPolicy;
  }
  /**
   * @param GoogleCloudAssetV1AnalyzeOrgPolicyGovernedAssetsResponseGovernedResource
   */
  public function setGovernedResource(GoogleCloudAssetV1AnalyzeOrgPolicyGovernedAssetsResponseGovernedResource $governedResource)
  {
    $this->governedResource = $governedResource;
  }
  /**
   * @return GoogleCloudAssetV1AnalyzeOrgPolicyGovernedAssetsResponseGovernedResource
   */
  public function getGovernedResource()
  {
    return $this->governedResource;
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
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAssetV1AnalyzeOrgPolicyGovernedAssetsResponseGovernedAsset::class, 'Google_Service_CloudAsset_GoogleCloudAssetV1AnalyzeOrgPolicyGovernedAssetsResponseGovernedAsset');
