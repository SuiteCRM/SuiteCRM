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

namespace Google\Service\TagManager;

class Container extends \Google\Collection
{
  protected $collection_key = 'usageContext';
  /**
   * @var string
   */
  public $accountId;
  /**
   * @var string
   */
  public $containerId;
  /**
   * @var string[]
   */
  public $domainName;
  protected $featuresType = ContainerFeatures::class;
  protected $featuresDataType = '';
  /**
   * @var string
   */
  public $fingerprint;
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $notes;
  /**
   * @var string
   */
  public $path;
  /**
   * @var string
   */
  public $publicId;
  /**
   * @var string[]
   */
  public $tagIds;
  /**
   * @var string
   */
  public $tagManagerUrl;
  /**
   * @var string[]
   */
  public $taggingServerUrls;
  /**
   * @var string[]
   */
  public $usageContext;

  /**
   * @param string
   */
  public function setAccountId($accountId)
  {
    $this->accountId = $accountId;
  }
  /**
   * @return string
   */
  public function getAccountId()
  {
    return $this->accountId;
  }
  /**
   * @param string
   */
  public function setContainerId($containerId)
  {
    $this->containerId = $containerId;
  }
  /**
   * @return string
   */
  public function getContainerId()
  {
    return $this->containerId;
  }
  /**
   * @param string[]
   */
  public function setDomainName($domainName)
  {
    $this->domainName = $domainName;
  }
  /**
   * @return string[]
   */
  public function getDomainName()
  {
    return $this->domainName;
  }
  /**
   * @param ContainerFeatures
   */
  public function setFeatures(ContainerFeatures $features)
  {
    $this->features = $features;
  }
  /**
   * @return ContainerFeatures
   */
  public function getFeatures()
  {
    return $this->features;
  }
  /**
   * @param string
   */
  public function setFingerprint($fingerprint)
  {
    $this->fingerprint = $fingerprint;
  }
  /**
   * @return string
   */
  public function getFingerprint()
  {
    return $this->fingerprint;
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
  public function setNotes($notes)
  {
    $this->notes = $notes;
  }
  /**
   * @return string
   */
  public function getNotes()
  {
    return $this->notes;
  }
  /**
   * @param string
   */
  public function setPath($path)
  {
    $this->path = $path;
  }
  /**
   * @return string
   */
  public function getPath()
  {
    return $this->path;
  }
  /**
   * @param string
   */
  public function setPublicId($publicId)
  {
    $this->publicId = $publicId;
  }
  /**
   * @return string
   */
  public function getPublicId()
  {
    return $this->publicId;
  }
  /**
   * @param string[]
   */
  public function setTagIds($tagIds)
  {
    $this->tagIds = $tagIds;
  }
  /**
   * @return string[]
   */
  public function getTagIds()
  {
    return $this->tagIds;
  }
  /**
   * @param string
   */
  public function setTagManagerUrl($tagManagerUrl)
  {
    $this->tagManagerUrl = $tagManagerUrl;
  }
  /**
   * @return string
   */
  public function getTagManagerUrl()
  {
    return $this->tagManagerUrl;
  }
  /**
   * @param string[]
   */
  public function setTaggingServerUrls($taggingServerUrls)
  {
    $this->taggingServerUrls = $taggingServerUrls;
  }
  /**
   * @return string[]
   */
  public function getTaggingServerUrls()
  {
    return $this->taggingServerUrls;
  }
  /**
   * @param string[]
   */
  public function setUsageContext($usageContext)
  {
    $this->usageContext = $usageContext;
  }
  /**
   * @return string[]
   */
  public function getUsageContext()
  {
    return $this->usageContext;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(Container::class, 'Google_Service_TagManager_Container');
