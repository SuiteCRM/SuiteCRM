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

class MastheadAd extends \Google\Collection
{
  protected $collection_key = 'companionYoutubeVideos';
  /**
   * @var string
   */
  public $autoplayVideoDuration;
  /**
   * @var string
   */
  public $autoplayVideoStartMillisecond;
  /**
   * @var string
   */
  public $callToActionButtonLabel;
  /**
   * @var string
   */
  public $callToActionFinalUrl;
  /**
   * @var string
   */
  public $callToActionTrackingUrl;
  protected $companionYoutubeVideosType = YoutubeVideoDetails::class;
  protected $companionYoutubeVideosDataType = 'array';
  /**
   * @var string
   */
  public $description;
  /**
   * @var string
   */
  public $headline;
  /**
   * @var bool
   */
  public $showChannelArt;
  protected $videoType = YoutubeVideoDetails::class;
  protected $videoDataType = '';
  /**
   * @var string
   */
  public $videoAspectRatio;

  /**
   * @param string
   */
  public function setAutoplayVideoDuration($autoplayVideoDuration)
  {
    $this->autoplayVideoDuration = $autoplayVideoDuration;
  }
  /**
   * @return string
   */
  public function getAutoplayVideoDuration()
  {
    return $this->autoplayVideoDuration;
  }
  /**
   * @param string
   */
  public function setAutoplayVideoStartMillisecond($autoplayVideoStartMillisecond)
  {
    $this->autoplayVideoStartMillisecond = $autoplayVideoStartMillisecond;
  }
  /**
   * @return string
   */
  public function getAutoplayVideoStartMillisecond()
  {
    return $this->autoplayVideoStartMillisecond;
  }
  /**
   * @param string
   */
  public function setCallToActionButtonLabel($callToActionButtonLabel)
  {
    $this->callToActionButtonLabel = $callToActionButtonLabel;
  }
  /**
   * @return string
   */
  public function getCallToActionButtonLabel()
  {
    return $this->callToActionButtonLabel;
  }
  /**
   * @param string
   */
  public function setCallToActionFinalUrl($callToActionFinalUrl)
  {
    $this->callToActionFinalUrl = $callToActionFinalUrl;
  }
  /**
   * @return string
   */
  public function getCallToActionFinalUrl()
  {
    return $this->callToActionFinalUrl;
  }
  /**
   * @param string
   */
  public function setCallToActionTrackingUrl($callToActionTrackingUrl)
  {
    $this->callToActionTrackingUrl = $callToActionTrackingUrl;
  }
  /**
   * @return string
   */
  public function getCallToActionTrackingUrl()
  {
    return $this->callToActionTrackingUrl;
  }
  /**
   * @param YoutubeVideoDetails[]
   */
  public function setCompanionYoutubeVideos($companionYoutubeVideos)
  {
    $this->companionYoutubeVideos = $companionYoutubeVideos;
  }
  /**
   * @return YoutubeVideoDetails[]
   */
  public function getCompanionYoutubeVideos()
  {
    return $this->companionYoutubeVideos;
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
  public function setHeadline($headline)
  {
    $this->headline = $headline;
  }
  /**
   * @return string
   */
  public function getHeadline()
  {
    return $this->headline;
  }
  /**
   * @param bool
   */
  public function setShowChannelArt($showChannelArt)
  {
    $this->showChannelArt = $showChannelArt;
  }
  /**
   * @return bool
   */
  public function getShowChannelArt()
  {
    return $this->showChannelArt;
  }
  /**
   * @param YoutubeVideoDetails
   */
  public function setVideo(YoutubeVideoDetails $video)
  {
    $this->video = $video;
  }
  /**
   * @return YoutubeVideoDetails
   */
  public function getVideo()
  {
    return $this->video;
  }
  /**
   * @param string
   */
  public function setVideoAspectRatio($videoAspectRatio)
  {
    $this->videoAspectRatio = $videoAspectRatio;
  }
  /**
   * @return string
   */
  public function getVideoAspectRatio()
  {
    return $this->videoAspectRatio;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(MastheadAd::class, 'Google_Service_DisplayVideo_MastheadAd');
