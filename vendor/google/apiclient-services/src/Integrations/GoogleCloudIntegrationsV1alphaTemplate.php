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

namespace Google\Service\Integrations;

class GoogleCloudIntegrationsV1alphaTemplate extends \Google\Collection
{
  protected $collection_key = 'tags';
  /**
   * @var string
   */
  public $author;
  /**
   * @var string[]
   */
  public $categories;
  protected $componentsType = GoogleCloudIntegrationsV1alphaTemplateComponent::class;
  protected $componentsDataType = 'array';
  /**
   * @var string
   */
  public $createTime;
  /**
   * @var string
   */
  public $description;
  /**
   * @var string
   */
  public $displayName;
  /**
   * @var string
   */
  public $docLink;
  /**
   * @var string
   */
  public $lastUsedTime;
  /**
   * @var string
   */
  public $name;
  /**
   * @var string[]
   */
  public $sharedWith;
  /**
   * @var string[]
   */
  public $tags;
  protected $templateBundleType = GoogleCloudIntegrationsV1alphaTemplateBundle::class;
  protected $templateBundleDataType = '';
  /**
   * @var string
   */
  public $updateTime;
  /**
   * @var string
   */
  public $usageCount;
  /**
   * @var string
   */
  public $usageInfo;
  /**
   * @var string
   */
  public $visibility;

  /**
   * @param string
   */
  public function setAuthor($author)
  {
    $this->author = $author;
  }
  /**
   * @return string
   */
  public function getAuthor()
  {
    return $this->author;
  }
  /**
   * @param string[]
   */
  public function setCategories($categories)
  {
    $this->categories = $categories;
  }
  /**
   * @return string[]
   */
  public function getCategories()
  {
    return $this->categories;
  }
  /**
   * @param GoogleCloudIntegrationsV1alphaTemplateComponent[]
   */
  public function setComponents($components)
  {
    $this->components = $components;
  }
  /**
   * @return GoogleCloudIntegrationsV1alphaTemplateComponent[]
   */
  public function getComponents()
  {
    return $this->components;
  }
  /**
   * @param string
   */
  public function setCreateTime($createTime)
  {
    $this->createTime = $createTime;
  }
  /**
   * @return string
   */
  public function getCreateTime()
  {
    return $this->createTime;
  }
  /**
   * @param string
   */
  public function setDescription($description)
  {
    $this->description = $description;
  }
  /**
   * @return string
   */
  public function getDescription()
  {
    return $this->description;
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
   * @param string
   */
  public function setDocLink($docLink)
  {
    $this->docLink = $docLink;
  }
  /**
   * @return string
   */
  public function getDocLink()
  {
    return $this->docLink;
  }
  /**
   * @param string
   */
  public function setLastUsedTime($lastUsedTime)
  {
    $this->lastUsedTime = $lastUsedTime;
  }
  /**
   * @return string
   */
  public function getLastUsedTime()
  {
    return $this->lastUsedTime;
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
   * @param string[]
   */
  public function setSharedWith($sharedWith)
  {
    $this->sharedWith = $sharedWith;
  }
  /**
   * @return string[]
   */
  public function getSharedWith()
  {
    return $this->sharedWith;
  }
  /**
   * @param string[]
   */
  public function setTags($tags)
  {
    $this->tags = $tags;
  }
  /**
   * @return string[]
   */
  public function getTags()
  {
    return $this->tags;
  }
  /**
   * @param GoogleCloudIntegrationsV1alphaTemplateBundle
   */
  public function setTemplateBundle(GoogleCloudIntegrationsV1alphaTemplateBundle $templateBundle)
  {
    $this->templateBundle = $templateBundle;
  }
  /**
   * @return GoogleCloudIntegrationsV1alphaTemplateBundle
   */
  public function getTemplateBundle()
  {
    return $this->templateBundle;
  }
  /**
   * @param string
   */
  public function setUpdateTime($updateTime)
  {
    $this->updateTime = $updateTime;
  }
  /**
   * @return string
   */
  public function getUpdateTime()
  {
    return $this->updateTime;
  }
  /**
   * @param string
   */
  public function setUsageCount($usageCount)
  {
    $this->usageCount = $usageCount;
  }
  /**
   * @return string
   */
  public function getUsageCount()
  {
    return $this->usageCount;
  }
  /**
   * @param string
   */
  public function setUsageInfo($usageInfo)
  {
    $this->usageInfo = $usageInfo;
  }
  /**
   * @return string
   */
  public function getUsageInfo()
  {
    return $this->usageInfo;
  }
  /**
   * @param string
   */
  public function setVisibility($visibility)
  {
    $this->visibility = $visibility;
  }
  /**
   * @return string
   */
  public function getVisibility()
  {
    return $this->visibility;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudIntegrationsV1alphaTemplate::class, 'Google_Service_Integrations_GoogleCloudIntegrationsV1alphaTemplate');
