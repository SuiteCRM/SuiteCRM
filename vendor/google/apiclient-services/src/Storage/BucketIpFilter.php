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

namespace Google\Service\Storage;

class BucketIpFilter extends \Google\Collection
{
  protected $collection_key = 'vpcNetworkSources';
  /**
   * @var string
   */
  public $mode;
  protected $publicNetworkSourceType = BucketIpFilterPublicNetworkSource::class;
  protected $publicNetworkSourceDataType = '';
  protected $vpcNetworkSourcesType = BucketIpFilterVpcNetworkSources::class;
  protected $vpcNetworkSourcesDataType = 'array';

  /**
   * @param string
   */
  public function setMode($mode)
  {
    $this->mode = $mode;
  }
  /**
   * @return string
   */
  public function getMode()
  {
    return $this->mode;
  }
  /**
   * @param BucketIpFilterPublicNetworkSource
   */
  public function setPublicNetworkSource(BucketIpFilterPublicNetworkSource $publicNetworkSource)
  {
    $this->publicNetworkSource = $publicNetworkSource;
  }
  /**
   * @return BucketIpFilterPublicNetworkSource
   */
  public function getPublicNetworkSource()
  {
    return $this->publicNetworkSource;
  }
  /**
   * @param BucketIpFilterVpcNetworkSources[]
   */
  public function setVpcNetworkSources($vpcNetworkSources)
  {
    $this->vpcNetworkSources = $vpcNetworkSources;
  }
  /**
   * @return BucketIpFilterVpcNetworkSources[]
   */
  public function getVpcNetworkSources()
  {
    return $this->vpcNetworkSources;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(BucketIpFilter::class, 'Google_Service_Storage_BucketIpFilter');
