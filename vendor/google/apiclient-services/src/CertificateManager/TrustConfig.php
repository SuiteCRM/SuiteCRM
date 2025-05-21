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

namespace Google\Service\CertificateManager;

class TrustConfig extends \Google\Collection
{
  protected $collection_key = 'trustStores';
  protected $allowlistedCertificatesType = AllowlistedCertificate::class;
  protected $allowlistedCertificatesDataType = 'array';
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
  public $etag;
  /**
   * @var string[]
   */
  public $labels;
  /**
   * @var string
   */
  public $name;
  protected $trustStoresType = TrustStore::class;
  protected $trustStoresDataType = 'array';
  /**
   * @var string
   */
  public $updateTime;

  /**
   * @param AllowlistedCertificate[]
   */
  public function setAllowlistedCertificates($allowlistedCertificates)
  {
    $this->allowlistedCertificates = $allowlistedCertificates;
  }
  /**
   * @return AllowlistedCertificate[]
   */
  public function getAllowlistedCertificates()
  {
    return $this->allowlistedCertificates;
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
  public function setEtag($etag)
  {
    $this->etag = $etag;
  }
  /**
   * @return string
   */
  public function getEtag()
  {
    return $this->etag;
  }
  /**
   * @param string[]
   */
  public function setLabels($labels)
  {
    $this->labels = $labels;
  }
  /**
   * @return string[]
   */
  public function getLabels()
  {
    return $this->labels;
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
   * @param TrustStore[]
   */
  public function setTrustStores($trustStores)
  {
    $this->trustStores = $trustStores;
  }
  /**
   * @return TrustStore[]
   */
  public function getTrustStores()
  {
    return $this->trustStores;
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
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(TrustConfig::class, 'Google_Service_CertificateManager_TrustConfig');
