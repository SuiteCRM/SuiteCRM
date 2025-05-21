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

class GoogleCloudSecuritycenterV2Resource extends \Google\Model
{
  protected $awsMetadataType = GoogleCloudSecuritycenterV2AwsMetadata::class;
  protected $awsMetadataDataType = '';
  protected $azureMetadataType = GoogleCloudSecuritycenterV2AzureMetadata::class;
  protected $azureMetadataDataType = '';
  /**
   * @var string
   */
  public $cloudProvider;
  /**
   * @var string
   */
  public $displayName;
  protected $gcpMetadataType = GcpMetadata::class;
  protected $gcpMetadataDataType = '';
  /**
   * @var string
   */
  public $location;
  /**
   * @var string
   */
  public $name;
  protected $resourcePathType = GoogleCloudSecuritycenterV2ResourcePath::class;
  protected $resourcePathDataType = '';
  /**
   * @var string
   */
  public $resourcePathString;
  /**
   * @var string
   */
  public $service;
  /**
   * @var string
   */
  public $type;

  /**
   * @param GoogleCloudSecuritycenterV2AwsMetadata
   */
  public function setAwsMetadata(GoogleCloudSecuritycenterV2AwsMetadata $awsMetadata)
  {
    $this->awsMetadata = $awsMetadata;
  }
  /**
   * @return GoogleCloudSecuritycenterV2AwsMetadata
   */
  public function getAwsMetadata()
  {
    return $this->awsMetadata;
  }
  /**
   * @param GoogleCloudSecuritycenterV2AzureMetadata
   */
  public function setAzureMetadata(GoogleCloudSecuritycenterV2AzureMetadata $azureMetadata)
  {
    $this->azureMetadata = $azureMetadata;
  }
  /**
   * @return GoogleCloudSecuritycenterV2AzureMetadata
   */
  public function getAzureMetadata()
  {
    return $this->azureMetadata;
  }
  /**
   * @param string
   */
  public function setCloudProvider($cloudProvider)
  {
    $this->cloudProvider = $cloudProvider;
  }
  /**
   * @return string
   */
  public function getCloudProvider()
  {
    return $this->cloudProvider;
  }
  /**
   * @param string
   */
  public function setDisplayName($displayName)
  {
    $this->displayName = $displayName;
  }
  /**
   * @return string
   */
  public function getDisplayName()
  {
    return $this->displayName;
  }
  /**
   * @param GcpMetadata
   */
  public function setGcpMetadata(GcpMetadata $gcpMetadata)
  {
    $this->gcpMetadata = $gcpMetadata;
  }
  /**
   * @return GcpMetadata
   */
  public function getGcpMetadata()
  {
    return $this->gcpMetadata;
  }
  /**
   * @param string
   */
  public function setLocation($location)
  {
    $this->location = $location;
  }
  /**
   * @return string
   */
  public function getLocation()
  {
    return $this->location;
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
   * @param GoogleCloudSecuritycenterV2ResourcePath
   */
  public function setResourcePath(GoogleCloudSecuritycenterV2ResourcePath $resourcePath)
  {
    $this->resourcePath = $resourcePath;
  }
  /**
   * @return GoogleCloudSecuritycenterV2ResourcePath
   */
  public function getResourcePath()
  {
    return $this->resourcePath;
  }
  /**
   * @param string
   */
  public function setResourcePathString($resourcePathString)
  {
    $this->resourcePathString = $resourcePathString;
  }
  /**
   * @return string
   */
  public function getResourcePathString()
  {
    return $this->resourcePathString;
  }
  /**
   * @param string
   */
  public function setService($service)
  {
    $this->service = $service;
  }
  /**
   * @return string
   */
  public function getService()
  {
    return $this->service;
  }
  /**
   * @param string
   */
  public function setType($type)
  {
    $this->type = $type;
  }
  /**
   * @return string
   */
  public function getType()
  {
    return $this->type;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudSecuritycenterV2Resource::class, 'Google_Service_SecurityCommandCenter_GoogleCloudSecuritycenterV2Resource');
