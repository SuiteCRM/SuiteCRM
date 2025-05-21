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

class GoogleCloudSecuritycenterV1Resource extends \Google\Collection
{
  protected $collection_key = 'folders';
  protected $awsMetadataType = AwsMetadata::class;
  protected $awsMetadataDataType = '';
  protected $azureMetadataType = AzureMetadata::class;
  protected $azureMetadataDataType = '';
  /**
   * @var string
   */
  public $cloudProvider;
  /**
   * @var string
   */
  public $displayName;
  protected $foldersType = Folder::class;
  protected $foldersDataType = 'array';
  /**
   * @var string
   */
  public $location;
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $organization;
  /**
   * @var string
   */
  public $parent;
  /**
   * @var string
   */
  public $parentDisplayName;
  /**
   * @var string
   */
  public $project;
  /**
   * @var string
   */
  public $projectDisplayName;
  protected $resourcePathType = ResourcePath::class;
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
   * @param AwsMetadata
   */
  public function setAwsMetadata(AwsMetadata $awsMetadata)
  {
    $this->awsMetadata = $awsMetadata;
  }
  /**
   * @return AwsMetadata
   */
  public function getAwsMetadata()
  {
    return $this->awsMetadata;
  }
  /**
   * @param AzureMetadata
   */
  public function setAzureMetadata(AzureMetadata $azureMetadata)
  {
    $this->azureMetadata = $azureMetadata;
  }
  /**
   * @return AzureMetadata
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
   * @param Folder[]
   */
  public function setFolders($folders)
  {
    $this->folders = $folders;
  }
  /**
   * @return Folder[]
   */
  public function getFolders()
  {
    return $this->folders;
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
   * @param string
   */
  public function setParent($parent)
  {
    $this->parent = $parent;
  }
  /**
   * @return string
   */
  public function getParent()
  {
    return $this->parent;
  }
  /**
   * @param string
   */
  public function setParentDisplayName($parentDisplayName)
  {
    $this->parentDisplayName = $parentDisplayName;
  }
  /**
   * @return string
   */
  public function getParentDisplayName()
  {
    return $this->parentDisplayName;
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
  /**
   * @param string
   */
  public function setProjectDisplayName($projectDisplayName)
  {
    $this->projectDisplayName = $projectDisplayName;
  }
  /**
   * @return string
   */
  public function getProjectDisplayName()
  {
    return $this->projectDisplayName;
  }
  /**
   * @param ResourcePath
   */
  public function setResourcePath(ResourcePath $resourcePath)
  {
    $this->resourcePath = $resourcePath;
  }
  /**
   * @return ResourcePath
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
class_alias(GoogleCloudSecuritycenterV1Resource::class, 'Google_Service_SecurityCommandCenter_GoogleCloudSecuritycenterV1Resource');
