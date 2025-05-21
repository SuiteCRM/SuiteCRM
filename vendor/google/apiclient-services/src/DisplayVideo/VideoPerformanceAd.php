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

namespace Google\Service\DisplayVideo;

class VideoPerformanceAd extends \Google\Collection
{
  protected $collection_key = 'videos';
  /**
   * @var string[]
   */
  public $actionButtonLabels;
  protected $companionBannersType = ImageAsset::class;
  protected $companionBannersDataType = 'array';
  /**
   * @var string[]
   */
  public $customParameters;
  /**
   * @var string[]
   */
  public $descriptions;
  /**
   * @var string
   */
  public $displayUrlBreadcrumb1;
  /**
   * @var string
   */
  public $displayUrlBreadcrumb2;
  /**
   * @var string
   */
  public $domain;
  /**
   * @var string
   */
  public $finalUrl;
  /**
   * @var string[]
   */
  public $headlines;
  /**
   * @var string[]
   */
  public $longHeadlines;
  /**
   * @var string
   */
  public $trackingUrl;
  protected $videosType = YoutubeVideoDetails::class;
  protected $videosDataType = 'array';

  /**
   * @param string[]
   */
  public function setActionButtonLabels($actionButtonLabels)
  {
    $this->actionButtonLabels = $actionButtonLabels;
  }
  /**
   * @return string[]
   */
  public function getActionButtonLabels()
  {
    return $this->actionButtonLabels;
  }
  /**
   * @param ImageAsset[]
   */
  public function setCompanionBanners($companionBanners)
  {
    $this->companionBanners = $companionBanners;
  }
  /**
   * @return ImageAsset[]
   */
  public function getCompanionBanners()
  {
    return $this->companionBanners;
  }
  /**
   * @param string[]
   */
  public function setCustomParameters($customParameters)
  {
    $this->customParameters = $customParameters;
  }
  /**
   * @return string[]
   */
  public function getCustomParameters()
  {
    return $this->customParameters;
  }
  /**
   * @param string[]
   */
  public function setDescriptions($descriptions)
  {
    $this->descriptions = $descriptions;
  }
  /**
   * @return string[]
   */
  public function getDescriptions()
  {
    return $this->descriptions;
  }
  /**
   * @param string
   */
  public function setDisplayUrlBreadcrumb1($displayUrlBreadcrumb1)
  {
    $this->displayUrlBreadcrumb1 = $displayUrlBreadcrumb1;
  }
  /**
   * @return string
   */
  public function getDisplayUrlBreadcrumb1()
  {
    return $this->displayUrlBreadcrumb1;
  }
  /**
   * @param string
   */
  public function setDisplayUrlBreadcrumb2($displayUrlBreadcrumb2)
  {
    $this->displayUrlBreadcrumb2 = $displayUrlBreadcrumb2;
  }
  /**
   * @return string
   */
  public function getDisplayUrlBreadcrumb2()
  {
    return $this->displayUrlBreadcrumb2;
  }
  /**
   * @param string
   */
  public function setDomain($domain)
  {
    $this->domain = $domain;
  }
  /**
   * @return string
   */
  public function getDomain()
  {
    return $this->domain;
  }
  /**
   * @param string
   */
  public function setFinalUrl($finalUrl)
  {
    $this->finalUrl = $finalUrl;
  }
  /**
   * @return string
   */
  public function getFinalUrl()
  {
    return $this->finalUrl;
  }
  /**
   * @param string[]
   */
  public function setHeadlines($headlines)
  {
    $this->headlines = $headlines;
  }
  /**
   * @return string[]
   */
  public function getHeadlines()
  {
    return $this->headlines;
  }
  /**
   * @param string[]
   */
  public function setLongHeadlines($longHeadlines)
  {
    $this->longHeadlines = $longHeadlines;
  }
  /**
   * @return string[]
   */
  public function getLongHeadlines()
  {
    return $this->longHeadlines;
  }
  /**
   * @param string
   */
  public function setTrackingUrl($trackingUrl)
  {
    $this->trackingUrl = $trackingUrl;
  }
  /**
   * @return string
   */
  public function getTrackingUrl()
  {
    return $this->trackingUrl;
  }
  /**
   * @param YoutubeVideoDetails[]
   */
  public function setVideos($videos)
  {
    $this->videos = $videos;
  }
  /**
   * @return YoutubeVideoDetails[]
   */
  public function getVideos()
  {
    return $this->videos;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(VideoPerformanceAd::class, 'Google_Service_DisplayVideo_VideoPerformanceAd');
