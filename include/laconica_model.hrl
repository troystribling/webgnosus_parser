%% laconica information model

%% laconica site
-record(laconica_site, 
    {
        root_url,
        poll_frequency
    }
).

%% laconica status message
-record(laconica_status, 
    {
        status_id,
        created_at, 
        user_id,
        text,
        source,
        truncated, 
        in_reply_to_status_id, 
        in_reply_to_user_id, 
        favorited 
    }
).
               
%% laconica user
-record(laconica_user,
    {
        user_id,
        user_name, 
        screen_name, 
        location, 
        description, 
        profile_image_url, 
        url, 
        protected, 
        followers_count, 
        status, 
        profile_background_color, 
        profile_text_color, 
        profile_link_color, 
        profile_sidebar_fill_color, 
        profile_sidebar_border_color, 
        friends_count, 
        created_at, 
        favourites_count, 
        utc_offset, 
        time_zone, 
        following, 
        notifications, 
        statuses_count
    }
).
