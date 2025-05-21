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

class CloudAiPlatformTenantresourceInfraSpannerConfigCreateDatabaseOptions extends \Google\Model
{
  /**
   * @var string
   */
  public $cmekCloudResourceName;
  /**
   * @var string
   */
  public $cmekCloudResourceType;
  /**
   * @var string
   */
  public $cmekServiceName;

  /**
   * @param string
   */
  public function setCmekCloudResourceName($cmekCloudResourceName)
  {
    $this->cmekCloudResourceName = $cmekCloudResourceName;
  }
  /**
   * @return string
   */
  public function getCmekCloudResourceName()
  {
    return $this->cmekCloudResourceName;
  }
  /**
   * @param string
   */
  public function setCmekCloudResourceType($cmekCloudResourceType)
  {
    $this->cmekCloudResourceType = $cmekCloudResourceType;
  }
  /**
   * @return string
   */
  public function getCmekCloudResourceType()
  {
    return $this->cmekCloudResourceType;
  }
  /**
   * @param string
   */
  public function setCmekServiceName($cmekServiceName)
  {
    $this->cmekServiceName = $cmekServiceName;
  }
  /**
   * @return string
   */
  public function getCmekServiceName()
  {
    return $this->cmekServiceName;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(CloudAiPlatformTenantresourceInfraSpannerConfigCreateDatabaseOptions::class, 'Google_Service_Contentwarehouse_CloudAiPlatformTenantresourceInfraSpannerConfigCreateDatabaseOptions');
