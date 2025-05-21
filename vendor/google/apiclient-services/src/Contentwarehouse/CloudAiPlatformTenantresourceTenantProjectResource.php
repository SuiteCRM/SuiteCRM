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

namespace Google\Service\Contentwarehouse;

class CloudAiPlatformTenantresourceTenantProjectResource extends \Google\Collection
{
  protected $collection_key = 'tenantServiceAccounts';
  protected $cloudSqlInstancesType = CloudAiPlatformTenantresourceCloudSqlInstanceConfig::class;
  protected $cloudSqlInstancesDataType = 'array';
  protected $gcsBucketsType = CloudAiPlatformTenantresourceGcsBucketConfig::class;
  protected $gcsBucketsDataType = 'array';
  protected $iamPolicyBindingsType = CloudAiPlatformTenantresourceIamPolicyBinding::class;
  protected $iamPolicyBindingsDataType = 'array';
  protected $infraSpannerConfigsType = CloudAiPlatformTenantresourceInfraSpannerConfig::class;
  protected $infraSpannerConfigsDataType = 'array';
  /**
   * @var string
   */
  public $tag;
  protected $tenantProjectConfigType = CloudAiPlatformTenantresourceTenantProjectConfig::class;
  protected $tenantProjectConfigDataType = '';
  /**
   * @var string
   */
  public $tenantProjectId;
  /**
   * @var string
   */
  public $tenantProjectNumber;
  protected $tenantServiceAccountsType = CloudAiPlatformTenantresourceTenantServiceAccountIdentity::class;
  protected $tenantServiceAccountsDataType = 'array';

  /**
   * @param CloudAiPlatformTenantresourceCloudSqlInstanceConfig[]
   */
  public function setCloudSqlInstances($cloudSqlInstances)
  {
    $this->cloudSqlInstances = $cloudSqlInstances;
  }
  /**
   * @return CloudAiPlatformTenantresourceCloudSqlInstanceConfig[]
   */
  public function getCloudSqlInstances()
  {
    return $this->cloudSqlInstances;
  }
  /**
   * @param CloudAiPlatformTenantresourceGcsBucketConfig[]
   */
  public function setGcsBuckets($gcsBuckets)
  {
    $this->gcsBuckets = $gcsBuckets;
  }
  /**
   * @return CloudAiPlatformTenantresourceGcsBucketConfig[]
   */
  public function getGcsBuckets()
  {
    return $this->gcsBuckets;
  }
  /**
   * @param CloudAiPlatformTenantresourceIamPolicyBinding[]
   */
  public function setIamPolicyBindings($iamPolicyBindings)
  {
    $this->iamPolicyBindings = $iamPolicyBindings;
  }
  /**
   * @return CloudAiPlatformTenantresourceIamPolicyBinding[]
   */
  public function getIamPolicyBindings()
  {
    return $this->iamPolicyBindings;
  }
  /**
   * @param CloudAiPlatformTenantresourceInfraSpannerConfig[]
   */
  public function setInfraSpannerConfigs($infraSpannerConfigs)
  {
    $this->infraSpannerConfigs = $infraSpannerConfigs;
  }
  /**
   * @return CloudAiPlatformTenantresourceInfraSpannerConfig[]
   */
  public function getInfraSpannerConfigs()
  {
    return $this->infraSpannerConfigs;
  }
  /**
   * @param string
   */
  public function setTag($tag)
  {
    $this->tag = $tag;
  }
  /**
   * @return string
   */
  public function getTag()
  {
    return $this->tag;
  }
  /**
   * @param CloudAiPlatformTenantresourceTenantProjectConfig
   */
  public function setTenantProjectConfig(CloudAiPlatformTenantresourceTenantProjectConfig $tenantProjectConfig)
  {
    $this->tenantProjectConfig = $tenantProjectConfig;
  }
  /**
   * @return CloudAiPlatformTenantresourceTenantProjectConfig
   */
  public function getTenantProjectConfig()
  {
    return $this->tenantProjectConfig;
  }
  /**
   * @param string
   */
  public function setTenantProjectId($tenantProjectId)
  {
    $this->tenantProjectId = $tenantProjectId;
  }
  /**
   * @return string
   */
  public function getTenantProjectId()
  {
    return $this->tenantProjectId;
  }
  /**
   * @param string
   */
  public function setTenantProjectNumber($tenantProjectNumber)
  {
    $this->tenantProjectNumber = $tenantProjectNumber;
  }
  /**
   * @return string
   */
  public function getTenantProjectNumber()
  {
    return $this->tenantProjectNumber;
  }
  /**
   * @param CloudAiPlatformTenantresourceTenantServiceAccountIdentity[]
   */
  public function setTenantServiceAccounts($tenantServiceAccounts)
  {
    $this->tenantServiceAccounts = $tenantServiceAccounts;
  }
  /**
   * @return CloudAiPlatformTenantresourceTenantServiceAccountIdentity[]
   */
  public function getTenantServiceAccounts()
  {
    return $this->tenantServiceAccounts;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(CloudAiPlatformTenantresourceTenantProjectResource::class, 'Google_Service_Contentwarehouse_CloudAiPlatformTenantresourceTenantProjectResource');
